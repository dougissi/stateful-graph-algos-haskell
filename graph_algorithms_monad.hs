import Prelude as P
import Data.Set as S
import Data.IntMap.Strict as M
import Data.Maybe
import Control.Monad.State


type Graph = M.IntMap [Int]
type VisitedNodes = S.Set Int
type Traversal = [Int]
type TraversalState = (VisitedNodes, Traversal)


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
    let n1 = aux1 i1 g
        n2 = aux1 i2 g
        n1' = aux2 i2 n1
        n2' = aux2 i1 n2
    in M.insert i1 n1' (M.insert i2 n2' g)
    where aux1 i g = fromMaybe [] (M.lookup i g)
          aux2 i neighbors = if i `elem` neighbors
                                then neighbors
                                else i:neighbors


buildGraph :: [(Int, Int)] -> Graph
buildGraph pairs = reverseNeighbors (aux pairs M.empty)
    where
        aux [] g = g
        aux ((i1, i2):is) g = aux is (connect g i1 i2)
        reverseNeighbors = M.map reverse


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
                Just neighbors -> do
                    put (S.insert root visited, t ++ [root])  -- consider root to be visited
                    loop g neighbors

        loop _ [] = do
            (_, t) <- get
            return t

        loop g (i:is) = do
            (visited, t) <- get
            if i `S.notMember` visited
                then dfs g i
                else return t  -- do nothing
            loop g is

trav g root =
    let res = evalState (traverseGraph g root) (S.empty, [])
    in res

