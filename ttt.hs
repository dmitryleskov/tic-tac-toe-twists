import qualified Data.Set as Set
import Data.Set((\\))
import qualified Data.Map as Map
import Data.List(intersperse)
import Debug.Trace

data Cell = Cross | Nought | Blank deriving (Eq)
instance Show Cell where
    show Cross  = "X"
    show Nought = "O"
    show Blank  = "."

data Field = Field { crosses :: Set.Set Int,
                     noughts :: Set.Set Int,
                     blanks :: Set.Set Int }
                     
instance Show Field where
    show field = concat $ intersperse "\n" lines
      where
        lines = [ concat [ showCell (r * 3 + c + 1)  | c <- [0..2]] | r <- [0..2] ]
        showCell n
          | Set.member n (crosses field) = show Cross
          | Set.member n (noughts field) = show Nought
          | otherwise = show Blank

makeMove :: Field -> Int -> Cell -> Field    
makeMove field n Cross = field { crosses = Set.insert n (crosses field), blanks = Set.delete n (blanks field) }
makeMove field n Nought = field { noughts = Set.insert n (noughts field), blanks = Set.delete n (blanks field) }

blankField = Field { crosses = Set.empty, noughts = Set.empty, blanks = Set.fromList [1..9] }
{--
someField = Field { crosses = Set.fromList [1,4,5], noughts = Set.fromList [2,9] }
testField1 = Field { crosses = Set.fromList [1], noughts = Set.fromList [5] }
testField2 = Field { crosses = Set.fromList [2,8], noughts = Set.fromList [1] }
testField3 = Field { crosses = Set.fromList [1,2,7], noughts = Set.fromList [3,4,5] }
--}

-- Caching

encode :: Field -> Int
encode field =
    foldl encodeCell 0 [1..9]
  where
    encodeCell :: Int -> Int -> Int
    encodeCell acc n
      | Set.member n (blanks field)  = acc * 2
      | Set.member n (crosses field) = acc * 4 + 2
      | Set.member n (noughts field) = acc * 4 + 3

newtype Cache = Cache (Map.Map Int (Int, Maybe Int)) deriving (Show)
emptyCache :: Cache
emptyCache = Cache Map.empty
cacheLookup :: Field -> Cache -> Maybe (Int, Maybe Int)
cacheLookup field (Cache m) = Map.lookup (encode field) m
cacheInsert :: Field -> (Int, Maybe Int) -> Cache -> Cache
cacheInsert field entry (Cache m) = Cache (Map.insert (encode field) entry m)

winningRows :: [Set.Set Int]
winningRows = [Set.fromList [1,2,3],
               Set.fromList [4,5,6],
               Set.fromList [7,8,9],
               Set.fromList [1,4,7],
               Set.fromList [2,5,8],
               Set.fromList [3,6,9],
               Set.fromList [1,5,9],
               Set.fromList [3,5,7]]

gameOver :: Field -> Maybe Cell
gameOver field 
    | any (flip Set.isSubsetOf (crosses field)) winningRows = Just Cross
    | any (flip Set.isSubsetOf (noughts field)) winningRows = Just Nought
    | null (blanks field) = Just Blank
    | otherwise = Nothing

minimax :: Field -> Cell -> Int -> Int -> Cache -> (Int, Maybe Int, Cache)
minimax field movesNext alpha beta cache =
    case cacheLookup field cache of
        Just (score, move) -> (score, move, cache)
        Nothing -> (score, move, cacheInsert field (score, move) newCache)
  where
    (score, move, _, newCache) =
        case gameOver field of
        Just Blank -> (0, Nothing, 0, cache)
        Just Cross -> (1 + length (blanks field), Nothing, 0, cache)
        Just Nought -> (-1 - length (blanks field), Nothing, 0, cache)
        _ -> if movesNext == Cross
                 -- foldr picked over foldl due to its ability to terminate early
                 then foldr maximize (minBound :: Int, Nothing, alpha, cache) $ blanks field
                 else foldr minimize (maxBound :: Int, Nothing, beta, cache) $ blanks field 
               where
                 maximize n best@(bestScore, bestMove, alpha, oldCache)
                     | alpha >= beta = best
                     | otherwise = 
                        let (newScore, newMove, newCache) =
                                minimax (makeMove field n Cross) Nought alpha beta oldCache
                        in  if newScore > bestScore
                                then (newScore, Just n, max alpha newScore, newCache)
                                else (bestScore, bestMove, max alpha bestScore, newCache)
                 minimize n best@(bestScore, bestMove, beta, oldCache)
                     | beta <= alpha = best
                     | otherwise = 
                        let (newScore, newMove, newCache) =
                                minimax (makeMove field n Nought) Cross alpha beta oldCache
                        in  if newScore < bestScore
                                then (newScore, Just n, min beta newScore, newCache)
                                else (bestScore, bestMove, min beta newScore, newCache)
                            
step :: Field -> Int -> Field
step field _ =
    traceShow field $
    let player = if odd $ Set.size $ blanks field
                     then Cross
                     else Nought
        (score, move, Cache cache) = minimax field player (minBound :: Int) (maxBound :: Int) emptyCache
    in  traceShow (Map.size cache) $
        case move of
        Just n -> makeMove field n player
        Nothing -> field
                              
main = do
--    print testField3
  --  print $ availableMoves testField3
--    print $ minimax blankField Cross emptyCache
--    print $ minimax someField Nought
--    print $ minimax testField1 Cross
--    print $ minimax testField2 Nought
--    print $ minimax testField3 Cross
    
    print $ foldl step blankField [1..9]
--    print $ foldl step testField2 [4..9]

