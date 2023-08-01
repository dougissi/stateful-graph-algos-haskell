import Test.HUnit hiding (Node)
import Data.Map as M (fromList)
import GraphsCommon
import GraphAlgosMonad as GAM
import GraphAlgos as GA
import Parse


-- Graph Construction, Triangle
--
--     1
--    / \
--   2 - 3
--

triGraph :: Graph
triGraph = buildGraph [(1,2), (1,3), (2,3)]

triGraphExpected :: Graph
triGraphExpected = M.fromList [ (1,[2,3])
                               ,(2,[1,3])
                               ,(3,[1,2])]

triGraph2 :: Graph
triGraph2 = buildGraph [(1,2), (1,3), (2,3), (1,2)]  -- duplicate edge ignored

triGraph3 :: Graph
triGraph3 = buildGraph [(2,1), (1,3), (2,3)]  -- node ordering in edge doesn't matter

triGraph4:: Graph
triGraph4 = buildGraph [(1,3), (2,3), (1,2)]  -- different edge ordering should produce different neighbor orderings

triGraph4Expected :: Graph
triGraph4Expected =  M.fromList [ (1,[3,2])  -- notice neighbor orderings for nodes 1 and 2
                                 ,(2,[3,1])
                                 ,(3,[1,2])]

buildGraphTests :: Test
buildGraphTests = test [  "for building triangle graph trivial,"                 ~: triGraphExpected ~=? triGraph
                        , "for building triangle graph with dup edge,"           ~: triGraphExpected ~=? triGraph2
                        , "for building triangle graph with reverse node order," ~: triGraphExpected ~=? triGraph3
                        , "for building triangle graph neighbor order,"          ~: triGraph4Expected ~=? triGraph4]


-- Traversal and Shortest Path Lengths, Triangle
--
--     1
--    / \
--   2 - 3
--
-- Logic: shortest paths between all nodes should be of length 1

expectedTriangleTraversal :: [Node]
expectedTriangleTraversal = [1,2,3]

expectedTriangleShortestPathLens :: [(Node, [(Node, Int)])]
expectedTriangleShortestPathLens = [ (1,[(2,1),(3,1)])
                                    ,(2,[(1,1),(3,1)])
                                    ,(3,[(1,1),(2,1)])]

triangleTests :: Test
triangleTests = test [  "for monadic triangle traversal,"              ~: expectedTriangleTraversal ~=? GAM.traversal triGraph 1
                      , "for non-monadic triangle traversal,"          ~: expectedTriangleTraversal ~=? GA.traversal triGraph 1
                      , "for monadic triangle shorest path lens,"      ~: expectedTriangleShortestPathLens ~=? GAM.shortestPathLens triGraph
                      , "for non-monadic triangle shortest path lens," ~: expectedTriangleShortestPathLens ~=? GA.shortestPathLens triGraph]

-- Traversal and Shortest Path Lengths, Linear Graph
--
-- 1
--  \
--   2
--    \
--     3
--      \
--       4
--
-- Logic:
-- * traversal from node 1 should do each in order
-- * trivial shortest paths


linearGraph :: Graph
linearGraph = buildGraph [(1,2), (2,3), (3,4)]

expectedLinearTraversal :: [Node]
expectedLinearTraversal = [1,2,3,4]

expectedLinearShortestPathLens :: [(Node, [(Node, Int)])]
expectedLinearShortestPathLens = [ (1,[(2,1),(3,2),(4,3)])
                                  ,(2,[(1,1),(3,1),(4,2)])
                                  ,(3,[(1,2),(2,1),(4,1)])
                                  ,(4,[(1,3),(2,2),(3,1)])]

linearTests :: Test
linearTests = test [  "for monadic linear traversal,"              ~: expectedLinearTraversal ~=? GAM.traversal linearGraph 1
                    , "for non-monadic linear traversal,"          ~: expectedLinearTraversal ~=? GA.traversal linearGraph 1
                    , "for monadic linear shortest path lens,"     ~: expectedLinearShortestPathLens ~=? GAM.shortestPathLens linearGraph
                    , "for non-monadic linear shortest path lens," ~: expectedLinearShortestPathLens ~=? GA.shortestPathLens linearGraph]


-- Traversal and Shortest Path Lengths, Triangle and Rectangle (Disconnected)
--
--     1
--    / \
--   2 - 3    4 - 5
--            |   |
--            7 - 6
--
-- Logic:
-- * Traversal from node 1 should not include any nodes in the square
-- * Traversal from node 4 should not include any nodes in the triangle
-- * There should be no shortest paths between the nodes in the triangle and the nodes in the square, and vice versa.

triRecGraph :: Graph
triRecGraph = buildGraph[(1,2), (1,3), (2,3), (4,5), (5,6), (6,7), (7,4)]

expectedTriRecTraversal1 :: [Node]
expectedTriRecTraversal1 = [1,2,3]

expectedTriRecTraversal4 :: [Node]
expectedTriRecTraversal4 = [4,5,6,7]

expectedTriRecShortestPathLens :: [(Node, [(Node, Int)])]
expectedTriRecShortestPathLens = [(1,[(2,1),(3,1)])
                                  ,(2,[(1,1),(3,1)])
                                  ,(3,[(1,1),(2,1)])
                                  ,(4,[(5,1),(6,2),(7,1)])
                                  ,(5,[(4,1),(6,1),(7,2)])
                                  ,(6,[(4,2),(5,1),(7,1)])
                                  ,(7,[(4,1),(5,2),(6,1)])]

triRecTests :: Test
triRecTests = test [  "for monadic tri+rec traversal from 1,"         ~: expectedTriRecTraversal1 ~=? GAM.traversal triRecGraph 1
                    , "for non-monadic tri+rec traversal from 1,"   ~: expectedTriRecTraversal1 ~=? GA.traversal triRecGraph 1
                    , "for monadic tri+rec traversal from 4,"       ~: expectedTriRecTraversal4 ~=? GAM.traversal triRecGraph 4
                    , "for non-monadic tri+rec traversal from 4,"   ~: expectedTriRecTraversal4 ~=? GA.traversal triRecGraph 4
                    , "for monadic tri+rec shortest path lens,"     ~: expectedTriRecShortestPathLens ~=? GAM.shortestPathLens triRecGraph
                    , "for non-monadic tri+rec shortest path lens," ~: expectedTriRecShortestPathLens ~=? GA.shortestPathLens triRecGraph]


-- Traversal and Shortest Path Lengths, Unbalanced Kite
--
--       1
--     /   \
--    2     3
--    |     |
--    |     4
--     \   /
--       5
--       |
--       6
--       |
--       7
--
-- Logic:
-- * Shortest path from node 1 to node 7 should be through node 2, not node 3
-- * Traversal from node 1 should be via depth first search (recursion ordering based on order of edges/nodes fed to `buildGraph`)

kiteGraph :: Graph
kiteGraph = buildGraph [(1,2), (1,3), (3,4), (4,5), (2,5), (5,6), (6,7)]

expectedKiteTraversal :: [Node]
expectedKiteTraversal = [1,2,5,4,3,6,7]

expectedKiteShortestPathLens :: [(Node, [(Node, Int)])]
expectedKiteShortestPathLens = [ (1,[(2,1),(3,1),(4,2),(5,2),(6,3),(7,4)])
                                ,(2,[(1,1),(3,2),(4,2),(5,1),(6,2),(7,3)])
                                ,(3,[(1,1),(2,2),(4,1),(5,2),(6,3),(7,4)])
                                ,(4,[(1,2),(2,2),(3,1),(5,1),(6,2),(7,3)])
                                ,(5,[(1,2),(2,1),(3,2),(4,1),(6,1),(7,2)])
                                ,(6,[(1,3),(2,2),(3,3),(4,2),(5,1),(7,1)])
                                ,(7,[(1,4),(2,3),(3,4),(4,3),(5,2),(6,1)])]

kiteTests :: Test
kiteTests = test [  "for monadic unbalanced kite traversal,"                ~: expectedKiteTraversal ~=? GAM.traversal kiteGraph 1
                  , "for non-monadic unbalanced kite traversal,"          ~: expectedKiteTraversal ~=? GA.traversal kiteGraph 1
                  , "for monadic unbalanced kite shorest path lens,"      ~: expectedKiteShortestPathLens ~=? GAM.shortestPathLens kiteGraph
                  , "for non-monadic unbalanced kite shortest path lens," ~: expectedKiteShortestPathLens ~=? GA.shortestPathLens kiteGraph]


-- Parse Edges File Contents
goodContents :: String
goodContents = "(1,2)\n(2,3)\n"

extraInt :: String
extraInt = "(1,2)\n(2,3,4)\n"

oneLessInt :: String
oneLessInt = "(1,2)\n(2)\n"

nonInt :: String
nonInt = "(1,2)\n(2,node3)\n"

twoBadLines :: String
twoBadLines = "(1, 2)\n(2, 3, 4)\n(5, )\n"

parseTests :: Test
parseTests = test [  "for edges file with good contents," ~: Right [(1,2),(2,3)] ~=? parseEdgesFile goodContents
                   , "for edges file with extra int,"     ~: Left "Error; these strings cannot be converted to (Int,Int): '(2,3,4)'" ~=? parseEdgesFile extraInt
                   , "for edges file with one less int,"  ~: Left "Error; these strings cannot be converted to (Int,Int): '(2)'" ~=? parseEdgesFile oneLessInt
                   , "for edges file with non-int,"       ~: Left "Error; these strings cannot be converted to (Int,Int): '(2,node3)'" ~=? parseEdgesFile nonInt
                   , "for edges file with 2 bad lines,"   ~: Left "Error; these strings cannot be converted to (Int,Int): '(2, 3, 4)', '(5, )'" ~=? parseEdgesFile twoBadLines]



-- Run Tests
tests :: Test
tests = test [buildGraphTests, linearTests, triangleTests, triRecTests, kiteTests, parseTests]

main :: IO Counts
main = runTestTT tests
