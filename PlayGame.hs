module PlayGame (playGameMain) where

import ConnectFourWithTwist
import Inf2d1
import Control.Exception
import Debug.Trace
import Data.Function (on)
import Data.List (sortBy)
-- import Data.List.Split You DO NOT need this!
import Data.List

playGameMain = do
  putStrLn "Would you like play Connect Four? (1 for Yes; 2 for No)"
  getLine >>= parseInput

parseInput :: String -> IO ()
parseInput str = case (read str):: Int of
                   1 -> getMethod >>= (playGame initGame humanPlayer)
                   2 -> putStrLn "Ok, goodbye."
                   (_) -> putStrLn "That is not a valid option."

getMethod :: IO (Int,Int)
getMethod = do
  putStrLn "Which game search method do you want to use?"
  
  putStrLn "1. Alphabeta"
  putStrLn "2. Minimax (Optional or if you can't implement alphabeta)"
  getInt >>= (\i -> case i of
                      1 -> return (2,0)
                      2 -> return (1,0))

getInt :: IO Int
getInt = getLine >>= (\str -> catch ((readIO str):: IO Int)
                              (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException) ++ ".\nPlease enter a valid Int value:")
                                        getInt))

playGame :: Game -> Role -> (Int,Int) ->  IO ()
playGame game role (x,y)| terminal game = (drawGrid game) >> endGame game 
                           | role == humanPlayer = (drawGrid game) >> do {mv <- getMove game; t <- getTurn; playGame (playTurn (playMove game humanPlayer mv) t) (switch humanPlayer) (x,y)}
                           
                                                    
                           
                           | otherwise = if x == 1   --- Minimax is selected
                                 then playGame (compMove minimax game) humanPlayer (x,y)
                       else playGame (compMove alphabeta game) humanPlayer (x,y)  --- Alphabeta is selected

endGame :: Game -> IO ()
endGame game | checkWin game humanPlayer  = putStrLn "You have won."
             | checkWin game (switch humanPlayer) = putStrLn "You have lost ... although this presumably means that your game player was quite good, so in a wider context you have actually won. Well done!"
             | otherwise = putStrLn "Draw"


-- compMove generate the move of computer player, which should get the step that make the chance of human wining minimum
compMove::(Role->Game->Int)->Game->Game
compMove fn game | length (elemIndices emptyCell game) < searchSpace = snd $ head $ sortBy (compare `on` fst) $ map (\i-> (fn humanPlayer i, i)) (movesAndTurns game compPlayer)  -- looking for the minimum value for computer player
                 | otherwise = generalMove game compPlayer


generalMove:: Game-> Role -> Game
generalMove g p = if (1 == lenSave && lenWin > 1)
                      then newGame g p saveCellIndex
                        else newGame g p winCellIndex
          where newGame g p index = let (frontList, afterwardsList) = splitAt index g in frontList ++ (p : tail afterwardsList)
                (lenSave, saveCellIndex) = firstAvailCell g $ sortOn length $ indexesforLines (elemIndices (switch p) g)  (elemIndices p g)
                (lenWin, winCellIndex)  = firstAvailCell g $ sortOn length $ indexesforLines (elemIndices p g)  (elemIndices (switch p) g)    --- get the best index for player p to win
                indexesforLines ownCells hostileCells= map (\l -> l \\ (intersect l ownCells))  [availLine | availLine<- getLines, null $ intersect availLine hostileCells]  --- provide the indexes to move, which is closest to make a win line and not occupied by the other player



--- return the information of possible winning lines which are legal to move
--- output: (the length to win, the legal moves to make)
firstAvailCell :: Game-> [[Int]] -> (Int, Int)
firstAvailCell game lines =  go lines
  where go lines = if [] == lines then (0, head possibIndex)
                    else if ([] /= intersect (head lines) possibIndex)   -- get the legal moves in the first wining line.
                        then (length (head lines), head $ intersect (head lines) possibIndex)
                        else go (tail lines)   -- if the winning line does not contain leagal move, try rest lines.
        possibIndex = [i | i <- (elemIndices emptyCell game), isbottomCell game i]  -- get the indexes of all current legal moves
        -- getOneIndexMove indexes= do i <- randomRIO (0, length indexes - 1) return $ indexes !! i

getMoveInt g = do
  a <- getMove g ;      
  return a

getTurnInt = do
  b <- getTurn;
  return b 
getMoveAndTurn :: Game -> (IO (Int,Int), IO Int)
getMoveAndTurn g = ((getMoveInt g),getTurnInt)


getTurn :: IO Int
getTurn= do
  putStrLn "Do you want to turn the game to the left after your move? (1-yes, 0-no) "
  num <-getInt 
  case num of
      1 -> return 1
      0 -> return 0

getMove :: Game->IO (Int,Int)
getMove game = do
  putStrLn "Enter (row,column): "
  getPair >>= (\cell -> if (isMoveValid game humanPlayer cell)
                        then return (cell)
                        else putStrLn "Invalid move. Please enter a valid move." >> getMove game)

getPair :: IO (Int,Int)
getPair = getLine >>= (\pair -> catch((readIO pair) :: IO (Int,Int))
                                (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException) ++ ".\nPlease enter a valid (row,column)")
                                          getPair))
