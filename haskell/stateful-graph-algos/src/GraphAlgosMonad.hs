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
type Queue = [(Int, Depth)]
type StateBFS = (VisitedNodes, Queue)


traversalM :: Graph -> Node -> State TraversalState Traversal
traversalM g root = do
    dfs g root
    (_, t) <- get
    return t

    where
        dfs g root = do
            (v, t) <- get
            case M.lookup root g of
                Nothing        -> return t
                Just neighbors -> do
                    put (S.insert root v, t ++ [root])  -- consider root to be visited
                    loop g neighbors

        loop _ [] = do
            (_, t) <- get
            return t

        loop g (i:is) = do
            (v, t) <- get
            if i `S.notMember` v
                then dfs g i
                else return t  -- do nothing
            loop g is


traversal :: Graph -> Node -> Traversal
traversal g root =
    let res = evalState (traversalM g root) (S.empty, [])
    in res


bfsM :: Graph -> Node -> Node -> State StateBFS Int
bfsM g s e = do
    case M.lookup s g of
        Nothing -> return (-1)          -- start not in graph
        Just _  ->
            case M.lookup e g of
                Nothing -> return (-1)  -- end not in graph
                _       -> do           -- start and end both in graph
                    (v, q) <- get
                    put (v, q ++ [(s, 0)])
                    aux g e
    where
        aux g e = do
            (v, q) <- get
            case q of
                []           -> return (-1)  -- queue empty; fail
                ((i, d): q') -> do
                    if i == e                -- at end; return depth
                        then return d    
                    else if i `S.member` v   -- already visited: don't add to queue
                        then do put (v, q')
                                aux g e
                    else do                  -- haven't visited: append to queue
                        let Just neighbors = M.lookup i g
                            q'' = q' ++ [(i, d + 1) | i <- neighbors ]
                            v' = S.insert i v
                        put (v', q'')
                        aux g e


bfs :: Graph -> Node -> Node -> Int
bfs g s e = evalState (bfsM g s e) (S.empty, [])


shortestPathLens :: Graph -> [(Node, [(Node, Int)])]
shortestPathLens = shortestPathLensViaBFS bfs
