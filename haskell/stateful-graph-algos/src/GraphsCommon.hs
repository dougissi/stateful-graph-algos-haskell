module GraphsCommon where

import Prelude as P
import Data.Map as M ( empty, lookup, insert, map, Map, keys )
import Data.Maybe (fromMaybe)
import System.IO


type Node = Int
type Edge = (Node, Node)
type Neighbors = [Node]
type Graph = M.Map Node Neighbors


strToInt :: String -> Int
strToInt = read


makeIntPair :: [String] -> (Int, Int)
makeIntPair [s1,s2] = (strToInt s1, strToInt s2)


formatGraphFile :: String -> [Edge]
formatGraphFile s =
    let ls = lines s
        strPairs = P.map words ls
    in P.map makeIntPair strPairs


addEdge :: Graph -> Edge -> Graph
addEdge g (i1, i2) =
    let n1 = getNeighbors i1 g
        n2 = getNeighbors i2 g
        n1' = addNeighbor i2 n1
        n2' = addNeighbor i1 n2
    in M.insert i1 n1' (M.insert i2 n2' g)
    where getNeighbors i g = fromMaybe [] (M.lookup i g)
          addNeighbor i neighbors = if i `elem` neighbors
                                        then neighbors
                                        else i:neighbors


buildGraph :: [Edge] -> Graph
buildGraph edges = reverseNeighbors (aux edges M.empty)
    where aux [] g = g
          aux (e:es) g = aux es (addEdge g e)
          reverseNeighbors = M.map reverse


shortestPathLensFromStart :: Graph -> Node -> [Node] -> (Graph -> Node -> Node -> Int) -> [(Node, Int)]
shortestPathLensFromStart g s ks bfs = reverse (aux g s ks [])
    where
        aux g s [] acc     = acc
        aux g s (n:ns) acc = 
            let len = bfs g s n
                newAcc = case len of
                    (-1) -> acc  -- don't include -1 paths
                    _    -> (n, len) : acc
            in aux g s ns newAcc


shortestPathLens :: Graph -> (Graph -> Node -> Node -> Int) -> [(Node, [(Node, Int)])]
shortestPathLens g bfs =
    let ks = keys g
    in reverse (aux g ks ks [])
    where
        aux g ks [] acc = acc
        aux g ks (s:ns) acc = 
            let newPaths = shortestPathLensFromStart g s ns bfs
                smallerKs = [x | x <- ks, x < s]
                oldPaths = reverse (getOldPaths s acc smallerKs [])
            in aux g ks ns ((s, oldPaths ++ newPaths):acc)
        
        getOldPaths s sp [] acc = acc
        getOldPaths s sp (i:is) acc = 
            let (Just isp) = P.lookup i sp
                newAcc = case P.lookup s isp of
                    Just sisp -> (i, sisp) : acc
                    Nothing   -> acc
            in getOldPaths s sp is newAcc


run :: (Graph -> Node -> [Node]) -> (Graph -> Node -> Node -> Int) -> String -> IO ()
run traversal bfs edgesFilename = do
    file <- openFile edgesFilename ReadMode
    contents <- hGetContents file
    let edges = formatGraphFile contents
        graph = buildGraph edges

    print "traversal"
    print (traversal graph 1)

    print "shortest path lengths"
    print (shortestPathLens graph bfs)