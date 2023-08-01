module GraphAlgos (traversal, shortestPathLens) where

import GraphsCommon
import Prelude as P
import Data.Set as S
import Data.Map as M


type Traversal = [Node]


traversal :: Graph -> Node -> Traversal
traversal g src =
    let (_, t) = dfs src S.empty []
    in t
    where
        dfs root v t =
            case M.lookup root g of
                Nothing -> (v, t)
                Just nbrs ->
                    let v' = S.insert root v  -- mark root as visited
                        t' = t ++ [root]  -- add root to traversal
                    in loop nbrs v' t'  -- do dfs on nonvisited neighbors
        loop [] v t = (v, t)  -- no neighbors remaining
        loop (i:is) v t =
            if i `S.notMember` v
                then let (v', t') = dfs i v t  -- continue traversal
                     in loop is v' t'
                else loop is v t  -- try next neighbor


bfs :: Graph -> Node -> Node -> Int
bfs g s e =
    case M.lookup s g of
        Nothing -> -1              -- start not in graph
        Just _  ->
            case M.lookup e g of
                Nothing -> -1      -- end not in graph
                _       ->         -- start, end both in graph
                    let v = S.empty   -- initialize empty visited set
                        q = [(s, 0)]  -- initialize queue with start
                    in aux v q        -- process queue
    where
        aux _ []           = -1        -- queue empty; fail
        aux v ((i, d): q')             -- "pop" next node
            | i == e       = d         -- win; return depth
            | S.member i v = aux v q'  -- already visited; skip
            | otherwise    =
                case M.lookup i g of   -- get node's neighbors
                    Nothing -> aux v q'  -- none; skip
                    Just nbrs ->         -- add neighbors to queue
                        let q'' = q' ++ [(x, d + 1) | x <- nbrs]
                            v' = S.insert i v
                        in aux v' q''


shortestPathLens :: Graph -> [(Node, [(Node, Int)])]
shortestPathLens = shortestPathLensViaBFS bfs
