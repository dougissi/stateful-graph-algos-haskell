import GraphsCommon
import Prelude as P
import Data.Set as S
import Data.Map as M


traversal :: Graph -> Node -> [Node]
traversal g root = 
    let (_, traversal) = dfs g root S.empty []
    in traversal
    where
        dfs g root v t = 
            case M.lookup root g of
                Nothing -> (v, t)
                Just neighbors -> 
                    let v' = S.insert root v  -- consider root to be visited
                        t' = t ++ [root]
                    in loop g root neighbors v' t'
        loop g root [] v t = (v, t)
        loop g root (i:is) v t =
            if i `S.member` v
                then loop g root is v t
                else let (v', t') = dfs g i v t
                     in loop g root is v' t'

bfs :: Graph -> Node -> Node -> Int
bfs g s e = 
    case M.lookup s g of
        Nothing -> -1  -- start not in graph
        Just _  ->
            case M.lookup e g of
                Nothing -> -1  -- end not in graph
                _       -> aux g e S.empty [(s, 0)]
    where
        aux _ _ _ [] = -1      -- queue empty; never reached end
        aux g e v ((i, d): q)  -- pop queue
            | i == e    = d    -- done
            | otherwise =
                let Just neighbors = M.lookup i g
                in if i `S.member` v
                    then aux g e v q  -- already visited: don't add to queue
                    else              -- not yet visited: add to queue
                        let q' = q ++ [(i, d + 1) | i <- neighbors]
                            v' = S.insert i v  -- consider current node visited
                        in aux g e v' q'
