import Prelude as P
import Data.IntMap.Strict as M


data Node = Node { val :: Int, neighbors :: [Int], visited :: Bool }
type Graph = M.IntMap Node

instance Show Node where
    show (Node a neighbors visited) = "Node " ++ show a ++ " " ++ show neighbors ++ " visited: " ++ show visited


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
    let (neighbors1, visited1) = aux1 i1 g
        (neighbors2, visited2) = aux1 i2 g
        neighbors1' = aux2 i2 neighbors1
        neighbors2' = aux2 i1 neighbors2
        n1 = Node i1 neighbors1' visited1
        n2 = Node i2 neighbors2' visited2
    in M.insert i1 n1 (M.insert i2 n2 g)
    where aux1 i g = case M.lookup i g of
                        Nothing -> ([], False)
                        Just (Node _ neighbors visited) -> (neighbors, visited)
          aux2 i neighbors = if i `elem` neighbors
                                then neighbors
                                else i:neighbors


buildGraph :: [(Int, Int)] -> Graph
buildGraph pairs = reverseNeighbors (aux pairs M.empty)
    where 
        aux [] g = g
        aux ((i1, i2):is) g = aux is (connect g i1 i2)
        reverseNeighbors = M.map (\(Node x neighbors v) -> Node x (reverse neighbors) v)



traverseDFS :: Graph -> Int -> [Int]
traverseDFS g root = 
    let (_, traversal) = dfs g root []
    in traversal
    where
        dfs g root t = 
            case M.lookup root g of
                Nothing -> (g, t)
                Just (Node _ neighbors _) -> 
                    let g' = M.insert root (Node root neighbors True) g  -- consider root to be visited
                        t' = t ++ [root]
                    in loop g' root neighbors t'
        loop g root [] t = (g, t)
        loop g root (i:is) t =
            let (Just next) = M.lookup i g
            in if visited next
                then loop g root is t
                else let (g', t') = dfs g (val next) t
                     in loop g' root is t'
                                        
main = do
    contents <- getContents
    let edges = formatGraphFile contents
    print (buildGraph edges)


{-
ToDo
* generalize show for more than just Integers
-}