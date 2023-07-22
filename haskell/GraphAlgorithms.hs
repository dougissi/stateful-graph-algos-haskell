import BuildGraphs
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

bfs :: Graph -> Node -> Node -> Node
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


shortestPathsFromStart :: Graph -> Node -> [Node] -> [(Node, Node)]
shortestPathsFromStart g s ks = reverse (aux g s ks [])
    where
        aux g s [] o     = o
        aux g s (n:ns) o = aux g s ns ((n, bfs g s n) : o)


shortestPaths :: Graph -> [(Node, [(Node, Node)])]
shortestPaths g =
    let ks = keys g
    in reverse (aux g ks ks [])
    where
        aux g ks [] acc = acc
        aux g ks (s:ns) acc = 
            let newPaths = shortestPathsFromStart g s ns
                smallerKs = [x | x <- ks, x < s]
                oldPaths = reverse (getOldPaths s acc smallerKs [])
            in aux g ks ns ((s, oldPaths ++ newPaths):acc)
        
        getOldPaths s sp [] acc = acc
        getOldPaths s sp (i:is) acc = 
            let (Just isp) = P.lookup i sp
                (Just sisp) = P.lookup s isp
            in getOldPaths s sp is ((i, sisp):acc)

               
main = do
    contents <- getContents
    let edges = formatGraphFile contents
        graph = buildGraph edges

    print "traversal"
    print (traversal graph 1)

    print "shortest paths"
    print (shortestPaths graph)


{-
ToDo
* generalize show for more than just Nodeegers
* on multiple runs, the node `visited` state stays True
-}