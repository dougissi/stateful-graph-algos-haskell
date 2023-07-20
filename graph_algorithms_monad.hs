import BuildGraphs
import Data.Set as S
import Data.IntMap as M
import Control.Monad.State


type VisitedNodes = S.Set Int
type Traversal = [Int]
type TraversalState = (VisitedNodes, Traversal)


traversalM :: Graph -> Int -> State TraversalState Traversal
traversalM g root = do
    dfs g root
    (_, t) <- get
    return t

    where
        dfs g root = do
            (v, t) <- get
            case M.lookup root g of
                Nothing -> return t
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

traveral g root =
    let res = evalState (traversalM g root) (S.empty, [])
    in res

