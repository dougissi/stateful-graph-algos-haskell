import Prelude as P
import Data.Set as S
import Data.IntMap.Strict as M


data Node = Node { id :: Int, neighbors :: S.Set Int }
type Graph = M.IntMap Node
type GraphError = String

instance Show Node where
    show (Node a neighbors) = "Node " ++ show a ++ " " ++ show (S.toList neighbors)

 

makeNode :: Graph -> Int -> Either GraphError Graph
makeNode g i = 
    let n = Node i empty
    in case M.lookup i g of
        Nothing -> Right (M.insert i n g)
        Just _  -> Left (show n ++ " already in graph")


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
    let neighbors1 = case M.lookup i1 g of 
                        Nothing -> S.empty
                        Just (Node _ neighbors) -> neighbors
        neighbors2 = case M.lookup i2 g of
                        Nothing -> S.empty
                        Just (Node _ neighbors)  -> neighbors
        n1 = Node i1 (S.insert i2 neighbors1)
        n2 = Node i2 (S.insert i1 neighbors2)
    in M.insert i1 n1 (M.insert i2 n2 g)


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