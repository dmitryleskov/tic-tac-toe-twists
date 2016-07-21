import qualified Data.Set as Set
import Data.Set((\\))
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

move :: Field -> Int -> Cell -> Field    
move field n Cross = field { crosses = Set.insert n (crosses field), blanks = Set.delete n (blanks field) }
move field n Nought = field { noughts = Set.insert n (noughts field), blanks = Set.delete n (blanks field) }

initField = Field { crosses = Set.empty, noughts = Set.empty, blanks = Set.fromList [1..9] }
{--
someField = Field { crosses = Set.fromList [1,4,5], noughts = Set.fromList [2,9] }
testField1 = Field { crosses = Set.fromList [1], noughts = Set.fromList [5] }
testField2 = Field { crosses = Set.fromList [2,8], noughts = Set.fromList [1] }
testField3 = Field { crosses = Set.fromList [1,2,7], noughts = Set.fromList [3,4,5] }
--}

streaks :: [Set.Set Int]
streaks = [Set.fromList [1,2,3],
           Set.fromList [4,5,6],
           Set.fromList [7,8,9],
           Set.fromList [1,4,7],
           Set.fromList [2,5,8],
           Set.fromList [3,6,9],
           Set.fromList [1,5,9],
           Set.fromList [3,5,7]]

gameOver :: Field -> Maybe Cell
gameOver field 
    | any (flip Set.isSubsetOf (crosses field)) streaks = Just Cross
    | any (flip Set.isSubsetOf (noughts field)) streaks = Just Nought
    | (Set.size (crosses field)) + (Set.size (noughts field)) == 9 = Just Blank
    | otherwise = Nothing
   
minimax :: Field -> Cell -> (Int, Maybe Int)
minimax field movesNext =
    case gameOver field of
    Just Blank -> (0, Nothing)
    Just Cross -> (1 + length (blanks field), Nothing)
    Just Nought -> (-1 - length (blanks field), Nothing)
    _ -> if movesNext == Cross
             then foldr maximize (minBound :: Int, Nothing) $ blanks field
             else foldr minimize (maxBound :: Int, Nothing) $ blanks field 
                where
                  maximize n old@(bestScore, _) =
                      let (score, _) = minimax (move field n Cross) Nought
                      in  if score > bestScore
                              then (score, Just n)
                              else old
                  minimize n old@(bestScore, _) =
                      let (score, _) = minimax (move field n Nought) Cross
                      in  if score < bestScore
                              then (score, Just n)
                              else old
                        
step :: Field -> Int -> Field
step field _ =
    traceShow field $
    let player = if odd $ Set.size $ blanks field
                     then Cross
                     else Nought
        (score, m) = minimax field player
    in  case m of
        Just n -> move field n player
        Nothing -> field
                              
main = do
--    print testField3
  --  print $ availableMoves testField3
    print $ minimax initField Cross
--    print $ minimax someField Nought
--    print $ minimax testField1 Cross
--    print $ minimax testField2 Nought
--    print $ minimax testField3 Cross
    
    print $ foldl step initField [1..9]
--    print $ foldl step testField2 [4..9]

