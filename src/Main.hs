module Main(main, test) where

import Simulation
import MapParser
import MineMap

solution :: MineMap -> Route
solution _ = [Abort]

test :: FilePath -> Route -> IO ()
test f path = do s <- readFile f
                 case parseMap s of
                   Left e -> error e
                   Right from -> do
                     print $ "From: " ++ show from
                     let (reason, to) = walkToEnd from path
                     print $ "To: " ++ show to
                     print $ "Stopped because of: " ++ show reason
                     print $ "Score: " ++ show (score from path (reason, to))

main :: IO ()
main = do s <- getContents
          case parseMap s of
            Left e -> error e
            Right m -> putStrLn $ routeToString $ solution m
