module Main(main) where

import MapParser
import MineMap

import System.IO

solution :: MineMap -> Route
solution _ = [Abort]

main :: IO ()
main = do s <- hGetContents stdin
          case parseMap s of
            Left e -> error e
            Right m -> putStrLn $ routeToString $ solution m
