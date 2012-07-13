module MapParser
  ( MapParser
  , parseMap)
  where

import Control.Monad
import Data.Maybe
import MineMap

type MapParser = Either String

parseChar :: Char -> MapParser Cell
parseChar ' ' = return Empty
parseChar '*' = return Rock
parseChar '#' = return Wall
parseChar '\\' = return Lambda
parseChar '.' = return Earth
parseChar 'L' = return $ Lift Closed
parseChar 'O' = return $ Lift Open
parseChar 'R' = return Robot
parseChar c = fail $ "Invalid character in map: '" ++ [c] ++ "'"

parseMap :: String -> MapParser MineMap
parseMap = mkMap <=< foldM combine (Nothing, newMap (1,1))
           . concatMap mkpos . zip [1..] . reverse . lines
  where mkpos (y,l) = zipWith (\x c -> ((x,y), c)) [1..] l
        combine (rp, m) (p, c) = do
          c' <- parseChar c
          let m' = setCell m p c'
          case c' of
            Robot | isJust rp -> fail "Two robots in map"
                  | otherwise -> return (Just p, m')
            _ -> return (rp, m')
        mkMap (Nothing, _) = Left "No robot in map"
        mkMap (Just _, m) = Right m
