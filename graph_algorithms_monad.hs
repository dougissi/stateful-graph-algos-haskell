import Prelude as P
import Data.Set as S
import Data.IntMap.Strict as M
import Control.Monad.State


data Node = Node { val :: Int, neighbors :: [Int] }
type Graph = M.IntMap Node
type VisitedNodes = S.Set Int
type GraphState = Graph
type Traversal = [Int]
type TraversalState = (VisitedNodes, Traversal)

instance Show Node where
    show (Node a neighbors) = "Node " ++ show a ++ " " ++ show neighbors

strToInt :: String -> Int
strToInt = read


makeIntPair :: [String] -> (Int, Int)
makeIntPair [s1,s2] = (strToInt s1, strToInt s2)


formatGraphFile :: String -> [(Int, Int)]
formatGraphFile s = 
    let ls = lines s
        strPairs = P.map words ls
    in P.map makeIntPair strPairs


connect :: Graph -> Int -> Int -> Graph
connect g i1 i2 = 
    let neighbors1 = aux1 i1 g
        neighbors2 = aux1 i2 g
        neighbors1' = aux2 i2 neighbors1
        neighbors2' = aux2 i1 neighbors2
        n1 = Node i1 neighbors1'
        n2 = Node i2 neighbors2'
    in M.insert i1 n1 (M.insert i2 n2 g)
    where aux1 i g = case M.lookup i g of
                        Nothing -> []
                        Just (Node _ neighbors) -> neighbors
          aux2 i neighbors = if i `elem` neighbors
                                then neighbors
                                else i:neighbors


buildGraph :: [(Int, Int)] -> Graph
buildGraph pairs = reverseNeighbors (aux pairs M.empty)
    where 
        aux [] g = g
        aux ((i1, i2):is) g = aux is (connect g i1 i2)
        reverseNeighbors = M.map (\(Node x neighbors) -> Node x (reverse neighbors))
        

traverseGraph :: Graph -> Int -> State TraversalState Traversal
traverseGraph g root = do
    dfs g root
    (_, t) <- get
    return t

    where
        dfs g root = do
            (visited, t) <- get
            case M.lookup root g of
                Nothing -> return t
                Just (Node _ neighbors) -> do
                    put (S.insert root visited, t ++ [root])  -- consider root to be visited
                    loop g neighbors            
            
        loop _ [] = do
            (_, t) <- get
            return t

        loop g (i:is) = do
            (visited, t) <- get
            if i `elem` visited 
                then return t
                else dfs g i
            loop g is
    
