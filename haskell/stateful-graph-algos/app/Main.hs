module Main (main) where

import GraphsCommon
import GraphAlgos as GA
import GraphAlgosMonad as GAM
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


repl :: GraphAlgosState -> IO ()
repl state@(g, impl, t, spl) = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == "exit" then
        do putStrLn "Bye!"
           return ()
    else if input == "switch" then
        do let (impl', t', spl') = toggleImplementation impl
           putStrLn $ "switched to " ++ impl' ++ "ic implementation"
           repl (g, impl', t', spl')
    else if input == "viewGraph" then
        do print g
           repl state
    else if input == "buildGraph" then
        do putStr "enter a series of edges, such as `(1,2) (2,3)`:\n"
           hFlush stdout
           input <- getLine
           let edgesStrings = words input
               edges = map (\x -> read x :: Edge) edgesStrings
           repl (buildGraph edges, impl, t, spl)
    else if input == "buildGraphFromFile" then
        do putStr "enter relative filepath: "
           hFlush stdout
           edgesFilename <- getLine
           file <- openFile edgesFilename ReadMode
           contents <- hGetContents file
           let edges = formatEdgesFile contents
           repl (buildGraph edges, impl, t, spl)
    else if input == "traversal" then
        do putStr "specify source node: "
           hFlush stdout
           input <- getLine
           let src = read input :: Int
           print (t g src)
           repl state
    else if input == "shortestPathLens" then
        do print (spl g)
           repl state
    else do
        putStrLn "unrecognized input"
        repl state


main :: IO ()
main = do putStrLn "Welcome to the Stateful Graph Algos REPL!\n"
          putStrLn "Commands (don't include '* '): "
          putStrLn "* exit -> quit REPL"
          putStrLn "* switch -> switch between monadic and non-monadic implementations (default is monadic)"
          putStrLn "* buildGraph -> will be prompted to manually enter edges of the desired graph"
          putStrLn "* buildGraphFromFile -> will be prompted to enter the relative filepath to the txt file of graph edges"
          putStrLn "* traversal -> after then being prompted to the starting node, will print out the in-order traversal from that node"
          putStrLn "* shortestPathLens -> will print out the shortest paths between all nodes (except those where no path exists)"
          repl (emptyGraph, "monad", GAM.traversal, GAM.shortestPathLens)
    
