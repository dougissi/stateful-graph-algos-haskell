import Test.HUnit hiding (Node)
import GraphsCommon
import GraphAlgosStateful as GAS
import GraphAlgosPure as GAP

-- Linear
--
-- 1
--  \
--   2
--    \
--     3
--      \
--       4

linearGraph :: Graph
linearGraph = buildGraph [(1,2), (2,3), (3,4)]

expectedLinearTraversal :: [Node]
expectedLinearTraversal = [1, 2, 3, 4]

expectedLinearShortestPathLens :: [(Node, [(Node, Int)])]
expectedLinearShortestPathLens = [(1,[(2,1),(3,2),(4,3)]),(2,[(1,1),(3,1),(4,2)]),(3,[(1,2),(2,1),(4,1)]),(4,[(1,3),(2,2),(3,1)])]

tests :: Test
tests = test [  "for stateful linear traversal,"          ~: expectedLinearTraversal ~=? GAS.traversal linearGraph 1
              , "for pure linear traversal,"              ~: expectedLinearTraversal ~=? GAP.traversal linearGraph 1
              , "for stateful linear shortest path lens," ~: expectedLinearShortestPathLens ~=? shortestPathLens linearGraph GAS.bfs
              , "for pure linear shortest path lens,"     ~: expectedLinearShortestPathLens ~=? shortestPathLens linearGraph GAP.bfs]

main :: IO Counts
main = runTestTT tests