module Main (main) where

import GraphsCommon
import GraphAlgos as GA
import GraphAlgosMonad as GAM
import Text.Read (readMaybe)
import System.IO

type EdgeError = String
type AlgosImplementation = String
type TraversalFunc = Graph -> Node -> [Node]
type ShortestPathLensFunc = Graph -> [(Node, [(Node, Int)])]
type GraphAlgosState = (Graph, AlgosImplementation, TraversalFunc, ShortestPathLensFunc)


toggleImplementation :: String -> (String, TraversalFunc, ShortestPathLensFunc)
toggleImplementation curr = if curr == "monad" then
                                ("non-monad", GA.traversal, GA.shortestPathLens)
                            else
                                ("monad", GAM.traversal, GAM.shortestPathLens)


makeEdge :: String -> Either EdgeError Edge
makeEdge line = let ws = words line
                    is = map strToInt ws
                in case is of
                    [Just i1, Just i2] -> Right (i1, i2)
                    _        -> Left ("'" ++ line ++ "' cannot be converted to a pair of integers")
    where strToInt s = readMaybe s :: Maybe Int


parseEdgesFile :: String -> Either EdgeError [Edge]
parseEdgesFile contents =
    case aux (lines contents) [] of
        Right edges -> Right edges
        Left err    -> Left ("Edge file parsing error: " ++ err)
    where aux [] acc     = Right (reverse acc)
          aux (x:xs) acc = case makeEdge x of
                              Right edge -> aux xs (edge:acc)
                              Left err   -> Left err


repl :: GraphAlgosState -> IO ()
repl state@(g, impl, t, spl) = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let inputWords = words input
    case inputWords of
        ["exit"]               -> do putStrLn "Bye!"
                                     return ()
        ["switch"]             -> do let (impl', t', spl') = toggleImplementation impl
                                     putStrLn $ "switched to " ++ impl' ++ "ic implementation"
                                     repl (g, impl', t', spl')
        ["viewGraph"]          -> do print g
                                     repl state
        ["graph","f",filepath] -> do file <- openFile filepath ReadMode
                                     contents <- hGetContents file
                                     case parseEdgesFile contents of
                                        Right edges -> do let g' = buildGraph edges
                                                          putStrLn $ "graph successfully built: " ++ show g'
                                                          repl (g', impl, t, spl)
                                        Left err    -> do putStrLn err
                                                          repl state
        ("graph":edgesStrs)    -> do let edges = map (\x -> read x :: Edge) edgesStrs
                                         g' = buildGraph edges
                                     putStrLn $ "graph successfully built: " ++ show g'
                                     repl (g', impl, t, spl)
        ["traversal",srcStr]   -> do print (t g (read srcStr :: Int))
                                     repl state
        ["shortestPathLens"]   -> do print (spl g)
                                     repl state
        _                      -> do putStrLn "unrecognized input"
                                     repl state


main :: IO ()
main = do putStrLn "Welcome to the Stateful Graph Algos REPL!\n"
          putStrLn "Commands (don't include '* '): "
          putStrLn "* exit -> quit REPL"
          putStrLn "* switch -> switch between monadic and non-monadic implementations (default is monadic)"
          putStrLn "* graph `edges` -> build graph from `edges` which is of the form '(1,2) (2,3) ...'"
          putStrLn "* graph f `filepath` -> build graph from file at `filepath`"
          putStrLn "* viewGraph -> print out the current graph" 
          putStrLn "* traversal `src` -> print in-order traversal from `src` node"
          putStrLn "* shortestPathLens -> print out the shortest paths between all nodes (except those where no path exists)"
          repl (emptyGraph, "monad", GAM.traversal, GAM.shortestPathLens)
    
