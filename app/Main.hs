module Main (main) where

import GraphsCommon
import GraphAlgos as GA
import GraphAlgosMonad as GAM
import Parse
import System.IO

type AlgosImplementation = String
type TraversalFunc = Graph -> Node -> [Node]
type ShortestPathLensFunc = Graph -> [(Node, [(Node, Int)])]
type GraphAlgosState = (Graph, AlgosImplementation, TraversalFunc, ShortestPathLensFunc)


toggleImplementation :: String -> (String, TraversalFunc, ShortestPathLensFunc)
toggleImplementation curr = if curr == "monad" then
                                ("non-monad", GA.traversal, GA.shortestPathLens)
                            else
                                ("monad", GAM.traversal, GAM.shortestPathLens)


handleEdgesParse :: GraphAlgosState -> Either EdgesParseError [Edge] -> IO ()
handleEdgesParse state@(_, impl, t, spl) edgesParse = do 
    case edgesParse of
        Right edges -> do let g' = buildGraph edges
                          putStrLn $ viewGraph g'
                          repl (g', impl, t, spl)
        Left err    -> do putStrLn err
                          repl state


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
        ["viewGraph"]          -> do putStrLn $ viewGraph g
                                     repl state
        ["graph","f",filepath] -> do file <- openFile filepath ReadMode
                                     contents <- hGetContents file
                                     handleEdgesParse state (parseEdgesFile contents)
        ("graph":edgesStrs)    -> handleEdgesParse state (parseEdgesStrs edgesStrs)
        ["traversal",srcStr]   -> do print (t g (read srcStr :: Int))
                                     repl state
        ["shortestPathLens"]   -> do print (spl g)
                                     repl state
        _                      -> do putStrLn "unrecognized input"
                                     repl state


main :: IO ()
main = do putStrLn "Welcome to the Stateful Graph Algos in Haskell REPL!\n"
          putStrLn "Commands:"
          putStrLn "* exit -> quit REPL"
          putStrLn "* switch -> switch between monadic and non-monadic implementations (default is monadic)"
          putStrLn "* graph `edges` -> build graph from `edges` which is of the form '(1,2) (2,3) ...'"
          putStrLn "* graph f `filepath` -> build graph from file at `filepath`"
          putStrLn "* viewGraph -> print current graph"
          putStrLn "* traversal `src` -> print in-order traversal of current graph from `src` node"
          putStrLn "* shortestPathLens -> print shortest paths between all nodes of current graph (except those where no path exists)"
          repl (emptyGraph, "monad", GAM.traversal, GAM.shortestPathLens)
    
