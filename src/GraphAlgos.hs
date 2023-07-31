module GraphAlgos (traversal, shortestPathLens) where

import GraphsCommon
import Prelude as P
import Data.Set as S
import Data.Map as M


traversal :: Graph -> Node -> [Node]
traversal graph root = 
    let (_, trav) = dfs graph root S.empty []
    in trav
    where
        dfs g r v t = 
            case M.lookup r g of
                Nothing -> (v, t)
                Just neighbors -> 
                    let v' = S.insert r v  -- consider root to be visited
                        t' = t ++ [r]
                    in loop g r neighbors v' t'
        loop _ _ [] v t = (v, t)
        loop g r (i:is) v t =
            if i `S.member` v
                then loop g r is v t
                else let (v', t') = dfs g i v t
                     in loop g r is v' t'

bfs :: Graph -> Node -> Node -> Int
bfs graph start end = 
    case M.lookup start graph of
        Nothing -> -1  -- start not in graph
        Just _  ->
            case M.lookup end graph of
                Nothing -> -1  -- end not in graph
                _       -> aux graph end S.empty [(start, 0)] -- initialize empty set of visited node and with only the start node in the queue
    where aux _ _ _ [] = -1      -- queue empty; never reached end
          aux g e v ((i, d): q)  -- pop queue
              | i == e    = d    -- done
              | otherwise = 
                  case M.lookup i g of
                      Nothing -> aux g e v q  -- next node in queue not in graph; skip
                      Just neighbors -> if i `S.member` v then 
                                          aux g e v q  -- next node already visited; skip
                                        else           -- next node not yet visited; add to queue
                                          let q' = q ++ [(x, d + 1) | x <- neighbors]
                                              v' = S.insert i v  -- consider current node visited
                                          in aux g e v' q'


shortestPathLens :: Graph -> [(Node, [(Node, Int)])]
shortestPathLens = shortestPathLensViaBFS bfs
