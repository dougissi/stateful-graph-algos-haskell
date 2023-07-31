module Parse (parseEdgesFile) where

import GraphsCommon
import Text.Read (readMaybe)
import Data.List (intercalate)


type LineEdgeError = String
type EdgeFileParseError = String


makeEdge :: String -> Either LineEdgeError Edge
makeEdge line = let ws = words line
                    is = map strToInt ws
                in case is of
                    [Just i1, Just i2] -> Right (i1, i2)
                    _        -> Left line
    where strToInt s = readMaybe s :: Maybe Int


parseEdgesFile :: String -> Either EdgeFileParseError [Edge]
parseEdgesFile contents =
    case aux (lines contents) [] [] of
        Right edges -> Right edges
        Left errs   -> Left ("Error; these lines cannot be converted to pairs of integers: '" ++ intercalate "', '" errs ++ "'")
    where aux [] [] edges = Right (reverse edges)  -- if no errors, return edges
          aux [] errs _   = Left (reverse errs)    -- if any errors, return errors
          aux (x:xs) errs edges = case makeEdge x of
                                    Right edge -> aux xs errs (edge:edges)
                                    Left err   -> aux xs (err:errs) edges
