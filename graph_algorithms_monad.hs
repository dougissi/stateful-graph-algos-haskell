import BuildGraphs
import Data.Set as S
import Data.IntMap as M
import Control.Monad.State


type VisitedNodes = S.Set Int
type Traversal = [Int]
type TraversalState = (VisitedNodes, Traversal)


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

