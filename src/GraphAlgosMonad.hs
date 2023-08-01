module GraphAlgosMonad (traversal, shortestPathLens) where

import GraphsCommon
import Prelude as P 
import Data.Set as S
import Data.Map as M
import Control.Monad.Trans.State

type VisitedNodes = S.Set Node
type Traversal = [Node]
type TraversalState = (VisitedNodes, Traversal)
type Depth = Int
type Queue = [(Node, Depth)]
type StateBFS = (VisitedNodes, Queue)


traversalM :: Graph -> Node -> State TraversalState Traversal
traversalM g = dfs
    where
        dfs root = do
            (v, t) <- get
            case M.lookup root g of
                Nothing        -> return t
                Just neighbors -> do
                    let v' = S.insert root v  -- mark root as visited
                        t' = t ++ [root]  -- add root to traversal
                    put (v', t')
                    loop neighbors  -- do dfs on nonvisited neighbors

        loop [] = do (_, t) <- get  -- no remaining neighbors
                     return t

        loop (i:is) = do (v, t) <- get
                         _ <- if i `S.notMember` v 
                                then dfs i     -- continue traversal
                                else return t  -- do nothing
                         loop is  -- try next neighbor


traversal :: Graph -> Node -> Traversal
traversal g root = evalState (traversalM g root) (S.empty, [])


bfsM :: Graph -> Node -> Node -> State StateBFS Int
bfsM g s e = do
    case M.lookup s g of
        Nothing -> return (-1)          -- start not in graph
        Just _  ->
            case M.lookup e g of
                Nothing -> return (-1)  -- end not in graph
                _       -> do           -- start, end both in graph
                    (v, q) <- get
                    put (v, q ++ [(s, 0)])  -- add start to queue
                    aux
    where
        aux = do
            (v, q) <- get
            case q of
                []           -> return (-1)   -- queue empty; fail
                ((i, d): q') -> do            -- "pop" next node
                    if i == e                 -- win; return depth
                        then return d    
                    else if i `S.member` v    -- already visited; skip
                        then do put (v, q')
                                aux
                    else do
                        case M.lookup i g of  -- get node's neighbors
                            Nothing   -> do put (v, q')  -- none; skip
                                            aux
                            Just nbrs -> do   -- add neighbors to queue
                                let q'' = q' ++ [(x, d + 1) | x <- nbrs]
                                    v' = S.insert i v
                                put (v', q'')
                                aux


bfs :: Graph -> Node -> Node -> Int
bfs g s e = evalState (bfsM g s e) (S.empty, [])


shortestPathLens :: Graph -> [(Node, [(Node, Int)])]
shortestPathLens = shortestPathLensViaBFS bfs
