module GraphsCommon (Graph, Node, Edge, emptyGraph, buildGraph, shortestPathLensViaBFS) where

import Prelude as P
import Data.Map as M ( empty, lookup, insert, map, Map, keys )
import Data.Maybe (fromMaybe)


type Node = Int
type Edge = (Node, Node)
type Neighbors = [Node]
type Graph = M.Map Node Neighbors


emptyGraph :: Graph
emptyGraph = M.empty


addEdge :: Graph -> Edge -> Graph
addEdge graph (i1, i2) =
    let n1 = getNeighbors i1
        n2 = getNeighbors i2
        n1' = addNeighbor i2 n1
        n2' = addNeighbor i1 n2
    in M.insert i1 n1' (M.insert i2 n2' graph)
    where getNeighbors i = fromMaybe [] (M.lookup i graph)
          addNeighbor i neighbors = if i `elem` neighbors
                                        then neighbors
                                        else i:neighbors


buildGraph :: [Edge] -> Graph
buildGraph edges = aux edges M.empty
    where aux [] g = reverseNeighbors g
          aux (e:es) g = aux es (addEdge g e)
          reverseNeighbors = M.map reverse


shortestPathLensFromStart :: Graph -> Node -> [Node] -> (Graph -> Node -> Node -> Int) -> [(Node, Int)]
shortestPathLensFromStart g start neighbors bfs = aux neighbors []
    where aux [] acc     = reverse acc
          aux (n:ns) acc = let len = bfs g start n
                               newAcc = case len of
                                   -1 -> acc  -- skip -1 paths
                                   _  -> (n, len) : acc
                           in aux ns newAcc


shortestPathLensViaBFS :: (Graph -> Node -> Node -> Int) -> Graph -> [(Node, [(Node, Int)])]
shortestPathLensViaBFS bfs g =
    let ks = keys g
    in aux ks ks []
    where
        aux _ [] acc = reverse acc
        aux ks (n:ns) acc =
            let newPaths = shortestPathLensFromStart g n ns bfs
                smallerKeys = [x | x <- ks, x < n]
                oldPaths = reverse (getOldPaths n acc smallerKeys [])
            in aux ks ns ((n, oldPaths ++ newPaths):acc)
        
        getOldPaths _ _ [] acc = acc
        getOldPaths s sp (k:ks) acc = let newAcc = case P.lookup k sp of
                                                       Nothing  -> acc  -- no shortest paths from smaller key found (shouldn't happen)
                                                       Just ksp -> case P.lookup s ksp of  -- look for start in shortest paths for smaller key
                                                                       Nothing  -> acc  -- no previously calculated shortest path from smaller key to start
                                                                       Just sksp -> (k, sksp) : acc
                                      in getOldPaths s sp ks newAcc
