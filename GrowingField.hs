module GrowingField (
    Cell(..),
    Boundaries(..),
    Field(delta,deltaHash,boundaries,movesNext),
    Move,
    Delta,
    cell,
    width,
    height,
    availableMoves,
    tryMove,
    makeMove,
    blankField,
{--
    someField,

    testField1,
    testField2,
    testField3 --}
) where


import qualified Data.Map as Map
import Data.Map((!))
import qualified Data.Set as Set
import Data.List(intersperse, nub)
import Debug.Trace

win = 3

data Cell = Cross | Nought | Blank | Neighbour | Remote deriving(Eq, Ord)
instance Show Cell where
    show Cross  = "X"
    show Nought = "O"
    show Blank  = "."
    show Neighbour = "?"
    show Remote = " "

{--
As the field may grow in any direction, it is initially centered at (0,0)
and field coordinates are mapped to indiced as follows:
      .
      7  .
   8  2  6  .
9  3  0  1  5 13
  10  4 12
     11
     
Here, indices 5, 7, 9, 11, and 13 map to cells that are not part of the initial field.
--}

xyToIndex :: Int -> Int -> Int
xyToIndex 0 0 = 0
xyToIndex x y
    | x > 0 && y >= 0 = first + y
    | x <= 0 && y > 0 = first + radius + radius - y
    | x < 0 && y <= 0 = first + radius * 2 - y
    | otherwise = first + radius * 3 + radius + y
  where
    radius = (abs x) + (abs y)
    first = radius * (radius - 1) * 2 + 1

indexToXY :: Int -> (Int, Int)
indexToXY 0 = (0, 0)
indexToXY i =
    let radius = floor ((sqrt (2 * (fromIntegral i - 1) + 1) - 1) / 2) + 1
        first = radius * (radius - 1) * 2 + 1
        offset = i - first
        x = (-radius) + abs (offset - radius * 2)
        y = (-radius) + abs ((offset + radius * 3) `rem` (radius * 4) - radius * 2)
    in  (x, y)

data Boundaries = Boundaries {
                      minX :: Int, maxX :: Int,
                      minY :: Int, maxY :: Int
                  } deriving(Show)
width :: Boundaries -> Int
width b  = maxX b - minX b + 1
height :: Boundaries -> Int
height b = maxY b - minY b + 1

type MoveXY = ((Int, Int), (Int, Int))
type Move = (Int, Int)
moveXYToMove :: MoveXY -> Move
moveXYToMove ((x, y), (bx, by)) = (xyToIndex x y, xyToIndex bx by)

type Delta = Set.Set (Int, Cell, Int)

data Field = Field { base :: Map.Map Int Cell,  -- Playing field 
                     delta :: Delta,            -- Moves made since base case
                     deltaHash :: Int,          -- Hash for caching deltas
                     neighbours :: Set.Set Int, -- Indices of candidates for field expansion
                     boundaries :: Boundaries,  -- Bounding rectangle (includes neightbours)
                     movesNext :: Cell}         -- Who moves next (Cross or Nought)

instance Show Field where
    show field = concat $ intersperse "\n" lines
      where
        lines = [ concat [ showCell (x, y) |
                           x <- [minX $ boundaries field .. maxX $ boundaries field]] |
                           y <- [maxY $ boundaries field, (maxY $ boundaries field) - 1 .. minY $ boundaries field] ]
        showCell (x, y) =
            case Map.lookup i $ base field of
                Just cell -> show cell
                Nothing | Set.member i $ neighbours field -> show Neighbour
                _ -> show Remote
              where
                i = xyToIndex x y

cell :: (Int, Int) -> Field -> Cell
cell (x,y) field =
    case Map.lookup i $ base field of
        Just c -> c
        Nothing | Set.member i $ neighbours field -> Neighbour
        _ -> Remote
  where
    i = xyToIndex x y

blankField :: Field
blankField =
    Field { base = blanks,
            delta = Set.empty,
            deltaHash = 1,
            neighbours = findNeighbours blanks,
            boundaries = Boundaries { minX = -2, minY = -2, maxX = 2, maxY = 2 },
            movesNext = Cross }
  where
    blanks = Map.fromList (map (\((x, y), c) -> (xyToIndex x y, c))
                           [((x, y), Blank) | x <- [-1..1], y <- [-1..1]])
    findNeighbours f =
        foldr query Set.empty $ Map.keys f
      where
        query i ns = foldr Set.insert ns (filter notInField $ candidates i) 
        notInField i = Map.notMember i f
        candidates i = let (x, y) = indexToXY i
                       in  map (\(x,y) -> xyToIndex x y)
                               [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

availableMoves :: Field -> [Move]
availableMoves field =
    [ (moveIndex, blankIndex) | 
      moveIndex <- Map.keys (Map.filter (\c -> c == Blank) $ base field),
      blankIndex <- Set.toList $ neighbours field
    ]

availableMovesXY :: Field -> [MoveXY]
availableMovesXY field = error " Not Implemented"

tryMove :: Field -> Move -> Field
tryMove field (markIndex, blankIndex) =
    Field { base = Map.insert markIndex (movesNext field) $
                   Map.insert blankIndex Blank (base field),
            delta = newDelta,
            deltaHash = hash newDelta,
            neighbours = updateNeighbours (neighbours field) bx by,
            boundaries = updateBoundaries (boundaries field) bx by,
            movesNext = other $ movesNext field }
  where
    newDelta = Set.insert (markIndex, movesNext field, blankIndex) $ delta field
    hash delta =
        foldl hash' 0 $ Set.toAscList delta
      where
        hash' :: Int -> (Int, Cell, Int) -> Int
        hash' acc (m, Cross, b) = (acc + m + 1) * 1000 * b
        hash' acc (m, Nought, b) = (acc + m + 1) * b
    (bx, by) = indexToXY blankIndex
    updateNeighbours ns x y =
        foldl (flip Set.insert) (Set.delete (xyToIndex x y) ns) (filter notInField $ candidates x y)
      where
        notInField i = Map.notMember i $ base field
        candidates :: Int -> Int -> [Int]
        candidates x y =
            map (\(x,y) -> xyToIndex x y)
                 [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    updateBoundaries :: Boundaries -> Int -> Int -> Boundaries
    updateBoundaries b bx by =
        Boundaries { minX = min (bx - 1) $ minX b,
                     maxX = max (bx + 1) $ maxX b,
                     minY = min (by - 1) $ minY b,
                     maxY = max (by + 1) $ maxY b }
    other Cross = Nought
    other Nought = Cross
      
makeMove :: Field -> Move -> Field
makeMove field move =
    (tryMove field move) {
        delta = Set.empty,
        deltaHash = 0
    }

tryMoveXY :: Field -> MoveXY -> Field    
tryMoveXY field movexy =
    tryMove field $ moveXYToMove movexy

makeMoveXY :: Field -> MoveXY -> Field    
makeMoveXY field movexy =
    makeMove field $ moveXYToMove movexy
    
    
{--



move :: Field -> (Int, Int) -> Cell -> Field    
move (Field field) (x, y) cell 
    | cell == Cross || cell == Nought =
        case Map.lookup i field of
            Just Blank -> Field (Map.insert i cell field)
            Nothing -> error ("Move offfield: " ++ (show x) ++ " " ++ (show y))
            _ -> error "Move to occupied cell"
      where
        i = xyToIndex x y
move _ _ cell = error $ "Wrong cell type in move: " ++ (show cell)

--}
{--

someField = Field (Map.fromList [((-1,1), Cross),  ((0,1), Nought), ((1,1), Blank),
                                 ((-1,0), Cross),  ((0,0), Cross),  ((1,0), Blank),
                                 ((-1,-1), Blank), ((0,-1), Blank), ((1,-1), Nought)])

testField1 = Field (Map.fromList [((-1,1), Cross), ((0,1), Blank), ((1,1), Blank),
                                 ((-1,0), Blank),  ((0,0), Nought),  ((1,0), Blank),
                                 ((-1,-1), Blank), ((0,-1), Blank), ((1,-1), Blank)])

testField2 = Field (Map.fromList [((-1,1), Nought), ((0,1), Cross), ((1,1), Blank),
                                 ((-1,0), Blank),  ((0,0), Blank),  ((1,0), Blank),
                                 ((-1,-1), Blank), ((0,-1), Cross), ((1,-1), Blank)])

testField3 = Field (Map.fromList [((-1,1), Cross), ((0,1), Cross), ((1,1), Nought),
                                 ((-1,0), Nought),  ((0,0), Nought),  ((1,0), Blank),
                                 ((-1,-1), Cross), ((0,-1), Blank), ((1,-1), Blank)])

--}