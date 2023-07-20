module BuildGraphs where

import Prelude as P
import Data.IntMap.Strict as M
    ( empty, lookup, insert, map, IntMap )
import Data.Maybe (fromMaybe)


type Graph = M.IntMap [Int]


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
