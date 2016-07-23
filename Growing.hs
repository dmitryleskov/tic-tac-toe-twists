import Data.List(sort, group)
import qualified Data.Map as Map
import Data.Map((!))
import qualified Data.Set as Set
import Data.List(intersperse, nub)
import GrowingField
import Debug.Trace

win = 4

data Frame = Frame { crosses :: Int,
                     noughts :: Int,
                     blanks :: Int,
                     neighbours :: Int }
             deriving(Show)

remotes f = win - (crosses f) - (noughts f) - (neighbours f) - (blanks f)

updateCount cell delta frame =
    case cell of
        Cross -> frame { crosses = crosses frame + delta }
        Nought -> frame { noughts = noughts frame + delta }
        Blank -> frame { blanks = blanks frame + delta }
        Neighbour -> frame { neighbours = neighbours frame + delta }
        Remote -> frame
                    
-- Numbers of Cross and Nought moves to this Frame to win                     
ranks :: Frame -> (Maybe Int, Maybe Int)
ranks f = 
    case f of
        Frame{noughts = 0, crosses = 0} -> (Just rank, Just rank)
        Frame{noughts = 0} -> (Just rank, Nothing)
        Frame{crosses = 0} -> (Nothing, Just rank)
        _ -> (Nothing, Nothing)
  where rank = (blanks f) + (neighbours f) * 2 + (remotes f) * 5

bestRanks :: (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int)
bestRanks (c1, n1) (c2, n2) = (best c1 c2, best n1 n2)
  where
    best Nothing mr = mr
    best mr Nothing = mr
    best (Just r1) (Just r2) = Just $ min r1 r2

scan :: Field -> (Int, Int) -> (Int, Int) -> Int -> (Maybe Int, Maybe Int)
scan field start@(x, y) step@(dx, dy) count = 
    let frame = preScan win
    in  loop (ranks frame) start count frame 
  where
    preScan :: Int -> Frame
    preScan count =
        preScanLoop Frame { crosses = 0, noughts = 0, blanks = 0, neighbours = 0 } $ count
      where
        preScanLoop :: Frame -> Int -> Frame
        preScanLoop frame 0 = frame
        preScanLoop frame count =
            let newFrame = updateCount (cell (x + dx * (count - 1), y + dy * (count - 1)) field) 1 frame
            in  preScanLoop newFrame (count - 1)
    loop :: (Maybe Int, Maybe Int) -> (Int, Int) -> Int -> Frame -> (Maybe Int, Maybe Int)
    loop best _ 0 frame = {--trace ((show frame) ++ "\n") $ --} best
    loop best (x, y) count frame =
--        trace (show frame) $
        let newXY = (x + dx * win, y + dy * win)
            trailXY = (x, y)
            add = cell newXY field
            drop = cell trailXY field
            newFrame = updateCount drop (-1) $
                       updateCount add 1 $
                       frame 
            newBest = bestRanks best $ ranks newFrame
        in  loop newBest (x + dx, y + dy) (count-1) newFrame
            
data ScanRule = ScanRule { start :: Boundaries -> (Int, Int), -- Starting cell
                           outerStep :: (Int, Int),           -- Scan rows offset
                           outerCount :: Boundaries -> Int,   -- Number of scan rows
                           innerStep :: (Int, Int),           -- Frame offset
                           innerCount :: Boundaries -> Int }  -- Number of scan frames

scanRules = [-- Rows
             ScanRule { start = \b -> (minX b, minY b),
                        outerStep = (0, 1),
                        outerCount = \b -> height b,
                        innerStep = (1, 0),
                        innerCount = \b -> width b - win},
             -- Columns       
             ScanRule { start = \b -> (minX b, minY b),
                        outerStep = (1, 0),
                        outerCount = \b -> width b,
                        innerStep = (0, 1),
                        innerCount = \b -> height b - win},
             -- Main diagonal and diagonals below it
             ScanRule { start = \b -> (minX b, minY b),
                        outerStep = (1, 0),
                        outerCount = \b -> (width b - win),
                        innerStep = (1, 1),
                        innerCount = \b -> height b - win},
             -- Diagonals above main
             ScanRule { start = \b -> (minX b, minY b + 1),
                        outerStep = (0, 1),
                        outerCount = \b -> (height b - win - 1),
                        innerStep = (1, 1),
                        innerCount = \b -> width b - win},
             -- Antidiagonal and diagonals above it
             ScanRule { start = \b -> (minX b, maxY b),
                        outerStep = (1, 0),
                        outerCount = \b -> (width b - win),
                        innerStep = (1, -1),
                        innerCount = \b -> height b - win},
             -- Diagonals below the antidiagonal           
             ScanRule { start = \b -> (minX b, maxY b -1),
                        outerStep = (0, -1),
                        outerCount = \b -> (height b - win - 1),
                        innerStep = (1, -1),
                        innerCount = \b -> height b - win}
            ]
            
uniScan field rule =
    loop (Nothing, Nothing) ((start rule) b) (outerStep rule) ((outerCount rule) b)
  where
    b = boundaries field
    loop :: (Maybe Int, Maybe Int) -> (Int, Int) -> (Int, Int) -> Int -> (Maybe Int, Maybe Int)
    loop best _ _ 0 = best
    loop best xy@(x, y) step@(dx, dy) count =
        loop (bestRanks best $ scan field xy (innerStep rule) ((innerCount rule) b))
             (x + dx, y + dy)
             step
             (count -1)

scanField :: Field -> (Maybe Int, Maybe Int)
scanField field =
    foldr bestRanks (Nothing, Nothing) $ map (uniScan field) scanRules
    
assess :: Field -> (Int, Maybe Cell)
assess field =
    case scanField field of
    (Nothing, Nothing) -> (0, Nothing)
    (Just 0, _) -> (maxBound :: Int, Just Cross) -- Crosses won
    (_, Just 0) -> (minBound :: Int, Just Nought) -- Noughts won
    (Just c, Just n) -> (n - c, Nothing)

newtype Cache = Cache (Map.Map Int [(Delta, (Int, Maybe Move))]) deriving (Show)
emptyCache :: Cache
emptyCache = Cache Map.empty
cacheLookup :: Cache -> Int -> Delta -> Maybe (Int, Maybe Move)
cacheLookup (Cache m) hash delta =
    case Map.lookup hash m of
        Nothing -> -- trace "No hit" $
            Nothing
        Just bucket -> -- trace ("Hash hit: " ++ (show hash) ++ " " ++ (show $ length bucket) ++ show bucket) $
            lookup delta bucket
        
cacheInsert :: Cache -> Int -> Delta -> (Int, Maybe Move) -> Cache
cacheInsert (Cache m) hash delta entry =
    case Map.lookup hash m of
        Nothing -> Cache (Map.insert hash [(delta, entry)] m)
        Just bucket -> Cache (Map.insertWith (++) hash [(delta, entry)] m)

cacheStats :: Cache -> [(Int, Int)]
cacheStats (Cache m) =
    map (\ss -> (head ss, length ss)) (group $ sort  $ Map.fold collect [] m)
  where
    collect :: [(Delta, (Int, Maybe Move))] -> [Int] -> [Int]
    collect bucket stats =
        (foldl collect1 [] bucket) ++ stats
      where
        collect1 :: [Int] -> (Delta, (Int, Maybe Move)) -> [Int]
        collect1 acc (_, (score, _)) = score : acc

minimax :: Field -> Int -> Cache -> (Int, Maybe Move, Cache)
minimax field depth cache =
    case cacheLookup cache (deltaHash field) (delta field) of
        Just (score, move) -> (score, move, cache)
        Nothing -> minimax' field depth cache
  where   
    minimax' :: Field -> Int -> Cache -> (Int, Maybe Move, Cache)
    minimax' field depth cache =
        (score, move, cacheInsert newCache (deltaHash field) (delta field) (score, move))
      where
        (score, move, newCache) =
            --traceShow field $
            case assess field of
            (s, Just _) -> (s, Nothing, cache)
            (s, _) | depth == 0 -> (s, Nothing, cache)
            _ -> if movesNext field == Cross
                     then foldl maximize (minBound :: Int, Nothing, cache) $ availableMoves field
                     else foldl minimize (maxBound :: Int, Nothing, cache) $ availableMoves field 
                        where
                          maximize (bestScore, bestMove, oldCache) move =
                              let (newScore, newMove, newCache) =
                                      minimax (tryMove field move) (depth - 1) oldCache
                              in  if newScore > bestScore
                                      then (newScore, Just move, newCache)
                                      else (bestScore, bestMove, newCache)
                          minimize (bestScore, bestMove, oldCache) move =
                              let (newScore, newMove, newCache) =
                                      minimax (tryMove field move) (depth - 1) oldCache
                              in  if newScore < bestScore
                                      then (newScore, Just move, newCache)
                                      else (bestScore, bestMove, newCache)
                              
main = do
    --print $ minimax someField Nought
    --print $ minimax testField1 Cross
    --print $ minimax testField2 Nought
--    print testField3
  --  print $ availableMoves testField3
    --print $ minimax testField3 Cross
    print $ score
    print $ cacheStats cache
  where
    (score, _, cache) = minimax blankField 3 emptyCache

