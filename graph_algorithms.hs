import Prelude as P
import Data.IntMap.Strict as M


data Node = Node { id :: Int, neighbors :: [Int], nextIndex :: Int }
type Graph = M.IntMap Node
type GraphError = String

instance Show Node where
    show (Node a neighbors nextI) = "Node " ++ show a ++ " " ++ show neighbors ++ " nextI: " ++ show nextI

 

-- makeNode :: Graph -> Int -> Either GraphError Graph
-- makeNode g i = 
--     let n = Node i empty 0
--     in case M.lookup i g of
--         Nothing -> Right (M.insert i n g)
--         Just _  -> Left (show n ++ " already in graph")


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
    let (neighbors1, nextIndex1) = aux1 i1 g
        (neighbors2, nextIndex2) = aux1 i2 g
        neighbors1' = aux2 i2 neighbors1
        neighbors2' = aux2 i1 neighbors2
        n1 = Node i1 neighbors1' nextIndex1
        n2 = Node i2 neighbors2' nextIndex2
    in M.insert i1 n1 (M.insert i2 n2 g)
    where aux1 i g = case M.lookup i g of
                        Nothing -> ([], 0)
                        Just (Node _ neighbors nextIndex) -> (neighbors, nextIndex)
          aux2 i neighbors = if i `elem` neighbors
                                then neighbors
                                else i:neighbors


buildGraph :: [(Int, Int)] -> Graph
buildGraph pairs = aux pairs M.empty
    where 
        aux [] g = g
        aux ((i1, i2):is) g = aux is (connect g i1 i2)


-- traverseDFS :: Graph -> Int -> [Int]
-- traverseDFS g root = aux g root M.empty
--     where
--         aux g root visited = case M.lookup root visited of
--                                 Nothing

    
main = do
    contents <- getContents
    let edges = formatGraphFile contents
    print (buildGraph edges)


{-
ToDo
* default value in Node constructor
* generalize show for more than just Integers
-}