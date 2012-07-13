module Main(main) where

import MapParser
import MineMap

import System.IO
import qualified Data.Set as S

solution :: MineMap -> Route
solution m = (head $ pathsTo m start end) ++ [Abort]
    where start = robot m
          end   = head $ S.toList $ lambdas m

main :: IO ()
main = do s <- hGetContents stdin
          case parseMap s of
            Left e -> error e
            Right m -> putStrLn $ routeToString $ solution m

