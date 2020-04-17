-- Made by: Gagan Devagiri
-- For: Inf2d Assignment 1 2019-2020
-- 20th March 2020


module Inf2d1 where

import Data.List (sortBy, elemIndices, elemIndex)
import ConnectFourWithTwist

import Data.Function (on)
import Data.List (sortBy, nub)
import Data.List.Split (chunksOf)



-- Section 1: Uniform Search



-- The Node type defines the position of the agent on the graph.
-- The Branch type synonym defines the branch of search through the graph.
type Node = Int
type Branch = [Node]
type Graph = [Node]

numNodes::Int
numNodes = 4


--- | next function returns the continuations of input search branch through the graph
---   Returns an empty list if the input graph is empty or else returns the next values in a node of branch
---   worst-case time complexity: O(b)
next :: Branch -> Graph ->  [Branch]
next _ [] = []
                -- list comprehension that zips the ithNode and range(0,len(graph)) to get a list of branch
                -- It filters the index that have a cost of 0 (i.e. no directed edge from two nodes)
next branch g = [ index:branch  |  (cost, index) <- zip ithNode [0.. (length g-1)], cost /= 0 ]

            where
                -- Gets the ith Node in the nested graph based on the node (index) of branch's head
                ithNode =  nestedGraph g !! head branch


--- | Nested Graph is a helper function for next and cost functions
---   Returns the graph formatted in a list of list of Nodes
---   Worst-case time complexity: O(N)
nestedGraph :: Graph -> [[Node]]
nestedGraph gr  = chunksOf ( round $ sqrt $ fromIntegral $ length gr ) gr



-- | The checkArrival function returns true if the current location of the robot
---  is the destination, and false otherwise.
---  Time-complexity: O(1)
checkArrival :: Node -> Node -> Bool
checkArrival destination curNode = destination == curNode

-- | The explored function returns true if a given Node appears in the list of nodes.
---  Used as a helper function for all the search functions
---  Time-complexity: O(N)
explored :: Node-> [Node] ->Bool
explored point exploredList = or [ node == point | node <- exploredList ]
                            -- Performs list comprehension to find the node n in a set of explored nodes.


-- Section 3 Uniformed Search


-- | Breadth-First Search
--   breadthFirstSearch function searches for a destination node using a breadth first search order
--   It uses checkArrival and next functions to check if a node is a destination position and to get the next set of branches.
--   Time-complexity: O(b^d)
breadthFirstSearch :: Graph -> Node->(Branch ->Graph -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch _ _ _ [] _ = Nothing -- exhaustive base case; Returns 'Nothing' when the branch is empty
breadthFirstSearch g destination next (fstBranch:otherBranches) exploredList
        -- Base case:      Returns the branch if the current node is the destination node
        | checkArrival destination (head fstBranch)  = Just fstBranch
        -- Recursive case: Destination node wasn't found, search continues with an updated set of explored nodes and frontier
        | otherwise                                = breadthFirstSearch g destination next frontier exploredList'
            
            where
                -- Gets the next set of nodes for the fstBranch in graph g
                branch = next fstBranch g
                -- Updates the exploredList variable by adding the currentNode in the function
                exploredList' = (head fstBranch) : exploredList
                -- Appends the next set of nodes to the current set of branches if the current node isn't explored.
                frontier = otherBranches ++  if explored (head fstBranch) exploredList then [] else next fstBranch g

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
-- Time-complexity:  O(b^d)
depthLimitedSearch :: Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int -> [Node]-> Maybe Branch
depthLimitedSearch _ _ _ [] _ _ = Nothing
depthLimitedSearch g destination next (fstBranch:otherBranches) d exploredList

        -- base case:       returns the branch if its the destination node we need
        | checkArrival destination $ head fstBranch  =  Just $ unique fstBranch
        -- recursive case:  depth limit has been reached, initates checking other branches
        | depthLimitReached                          =  depthLimitedSearch g destination next otherBranches d exploredListCutOff
        -- recursive case:  Above cases weren't triggered, the search continues with a new updated set of branches and explored list.
        | otherwise                                  =  depthLimitedSearch g destination next frontier d exploredList'

            where
                -- Gets the next set of nodes for the fstBranch in graph g
                branch = next fstBranch g
                -- Updates the exploredList variable by adding the currentNode in the function
                exploredList' = head fstBranch : exploredList
                -- Appends the next set of nodes to the current set of branches at the end if the current node isn't explored.
                frontier = (if explored (head fstBranch) exploredList then [] else next fstBranch g) ++ otherBranches
                -- Checks if the depthLimit has been reached
                depthLimitReached = if length fstBranch - d - 1 == 0 then True else False
                -- Updates exploredList. (called when there's a cutoff) removes the fstBranch from the exploredList
                exploredListCutOff = [ node | node <-exploredList, not $ elem node fstBranch ]
                -- Used to give us a concise solution removing any loops in the graph such as [4,3,4,3,5] to [4,3,5]
                unique = reverse . nub . reverse






-- | Section 4: Informed search


-- | AStar Helper Functions

-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.
---  The cost of a whole trace is the sum of all relevant transition costs.
--- Time-Complexity: O(b) where b is the branch length
cost :: Graph ->Branch  -> Int
cost [] _ = 0
cost gr br = sum [ (nestedGraph gr !! nodeIndex ) !! valIndex | (valIndex, nodeIndex) <- zip br (tail br) ]
             -- Performs list comprehension


-- | The getHr function reads the heuristic for a node from a given heuristic table.
--   The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. 
--   It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  
--   Time-Complexity: O(1)
getHr :: [Int]->Node->Int
getHr hrTable node = hrTable !! node -- Indexes array at node

-- | A* Search
---- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,
---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.
---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.
---- Time - Complexity:
aStarSearch :: Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch [] _ _ _ _ _ _ _ = Nothing
aStarSearch _ _ _ _ _ _ [] _ = Nothing
aStarSearch g destination next getHr hrTable cost (fstBranch:otherBranches) exploredList 
        -- base case:       returns the branch if its the destination node we need
        | checkArrival destination $ head fstBranch  = Just fstBranch
        -- Recursive case: Destination node wasn't found, search continues with an updated set of explored nodes and branches sorted by cost
        | otherwise                                  = aStarSearch g destination next getHr hrTable cost branchesSorted exploredList'
            
            where
                -- Gets the next set of nodes for the fstBranch in graph g
                branch = next fstBranch g
                -- Updates the exploredList variable by adding the currentNode in the function
                exploredList' = (head fstBranch) : exploredList
                -- Appends the next set of nodes to the current set of branches at the end if the current node isn't explored.
                frontier = otherBranches ++  if explored (head fstBranch) exploredList then [] else next fstBranch g
                -- Priority queue that calculates cost with with `cost` and `hrTable` function. Returns a list of tuple of branch and cost 
                sortedFrontierWithCost = sortBy (compare `on` snd) [ (currentBranch, cost g currentBranch + getHr hrTable (currentBranch !! 0)) | currentBranch <- frontier ]
                -- Returns a priority queue of branches using the helper function above
                branchesSorted = [ branch | (branch, cost) <- sortedFrontierWithCost ]

-- | Section 5: Games
-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 



-- | Section 5.1 Connect Four with a Twist

 
-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game -> Int
eval game
    | terminal game     = if checkWin game 1 then 1 else (if checkWin game 0 then -1 else 0)
    | otherwise           = 0


-- | The alphabeta function should return the minimax value using alphabeta pruning.
--   The eval function should be used to get the value of a terminal state.
alphabeta:: Role -> Game -> Int
alphabeta player game
  -- v = maxValue(state, -inf, +inf)
  | maxPlayer == player = maxValue game (-2) 2 -- -inf and +inf are set to -2 and +2 respectively as described in the cw.
  -- minPlayer == player
  | otherwise = minValue game (-2) 2

-- | minValue is a helper function for alphabeta. It's equivalent to the function
--   Min-Value(state, alpha, beta) in the psuedocode
minValue ::Game -> Int -> Int -> Int
minValue game alpha beta
    | terminal game     = eval game
    | otherwise         = minLoopPart (movesAndTurns game 0) alpha beta 2


-- | minLoopPart is a helper function for alphabeta which describes the loop part 
--   of the minimum value function in the psuedocode
minLoopPart :: [Game] -> Int -> Int -> Int -> Int
minLoopPart [] alpha beta v = v
minLoopPart (fstGame:otherGames) alpha beta v
    -- if v' > beta = alpha <- min(alpha, v')
    | v' > alpha          = minLoopPart otherGames alpha (min beta v') v'
    | otherwise           = v'

      where
        -- v <- Min(v, Max-Value(Result(s,a)), alpha, beta )
        v' = min v $ maxValue fstGame alpha beta


-- | maxValue is a helper function for alphabeta. It's equivalent to the function
--   max-Value(state, alpha, beta) in the psuedocode.
maxValue :: Game -> Int -> Int -> Int
maxValue game alpha beta
  -- if terminal-test(state) then return utility(state)
  | terminal game = eval game
  -- v <- -inf (i.e. v <- -2) and go to loop part
  | otherwise = maxLoopPart (moves game 1) alpha beta (-2)


-- | maxLoopPart is a helper function for alphabeta which describes the loop part 
--   of the maximum-value function in the psuedocode
maxLoopPart :: [Game] -> Int -> Int -> Int -> Int
maxLoopPart [] alpha beta v = v
maxLoopPart (fstGame:otherGames) alpha beta v
  -- if v < beta = alpha <- max(alpha, v')
  | v' < beta = maxLoopPart otherGames (max alpha v') beta v'
  | otherwise = v'
      where 
        -- v' <- max(v, min-Value( result(s, a), alpha, beta))
        v' = max v $ minValue fstGame alpha beta



-- | OPTIONAL!
-- You can try implementing this as a test for yourself or if you find alphabeta pruning too hard.
-- If you implement minimax instead of alphabeta, the maximum points you can get is 10% instead of 15%.
-- Note, we will only grade this function IF YOUR ALPHABETA FUNCTION IS EMPTY.
-- The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.
minimax:: Role -> Game -> Int
minimax player game=undefined
{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}
