module Parse (EdgesParseError, parseEdgesStrs, parseEdgesFile) where

import GraphsCommon
import Text.Read (readMaybe)
import Data.List (intercalate)


type EdgeStrError = String
type EdgesParseError = String


makeEdge :: String -> Either EdgeStrError Edge
makeEdge line = case readMaybe line :: Maybe Edge of
                    Just edge -> Right edge
                    _         -> Left line

parseEdgesStrs :: [String] -> Either EdgesParseError [Edge]
parseEdgesStrs edgesStrs = 
    case aux edgesStrs [] [] of
        Right edges -> Right edges
        Left errs   -> Left ("Error; these strings cannot be converted to (Int,Int): '" ++ intercalate "', '" errs ++ "'")
    where aux [] [] edges = Right (reverse edges)  -- if no errors, return edges
          aux [] errs _   = Left (reverse errs)    -- if any errors, return errors
          aux (x:xs) errs edges = case makeEdge x of
                                    Right edge -> aux xs errs (edge:edges)
                                    Left err   -> aux xs (err:errs) edges

parseEdgesFile :: String -> Either EdgesParseError [Edge]
parseEdgesFile contents = parseEdgesStrs (lines contents)
