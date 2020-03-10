module GraphSearch (graphSearchMain) where

import Data.Array
import Data.Maybe (fromJust,isNothing)
import System.IO
import Control.Exception
import Inf2d1
import Debug.Trace




debug = flip trace
newtype Path = Path ()

instance Show Path where
    show pt = "x"

depthLimit = 40

-- you can change the graph, start- and endNode  and the heuristicTable variable to test out different graphs!
graph::Graph
graph=[0,1,2,0, 0,0,0,1, 0,0,0,0, 0,0,0,0]

heuristicTable::[Int]
heuristicTable=[30,9,4,0]


startNode::Node
startNode =0

endNode::Node
endNode=3



graphSearchMain = do

  method <- getMethod
  findPath startNode endNode graph method

findPath :: Int -> Int -> Graph  -> (Int,Int) -> IO ()
findPath start end graph method = putStrLn $ show path
    where search = case method of
                   (1,_) -> breadthFirstSearch graph end next [[start]]   [] 
                   (2,_) -> depthLimitedSearch  graph end next [[start]] depthLimit []              
                   (3,_) -> aStarSearch graph end next getHr heuristicTable cost [[start]] []
          path = case search of
                   Just x -> displayGraphPath x 
                   Nothing -> error "Your search function couldn't find a path."

displayGraphPath::[Node] -> String
displayGraphPath [] = ""
displayGraphPath (cur_node:node_list_rest) = (show cur_node) ++ "<-" ++ displayGraphPath node_list_rest

getMethod :: IO (Int,Int)
getMethod = do
  putStrLn "Which search method would you like to use?"
  putStrLn "1. Breadth First Search"
  putStrLn ("2. Depth Limited Search (Test at a depth limit of " ++ show(depthLimit) ++ ")")
  putStrLn "3. A* Search"
  num <- getNum
  case num of
    1 -> return (1,0)
    2 -> return (2,0)
    3 -> return (3,0)

    (_) -> do putStrLn "Please enter a number in the range [1-3]"
              getMethod

getNum :: IO Int
getNum = do
  str <- getLine
  num <- catch((readIO str)::IO Int)
          (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException) ++ ".\nPlease enter a valid Int value:")
                    getNum)
  if not (elem num [1..6])
     then do putStrLn "Please enter a number in the range [1-3]"
             getNum
     else return num



