module MapParser
  ( MapParser
  , parseMap)
  where

import Control.Monad
import Data.Char
import Data.List
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
parseMap s = do metadata <- parseMetadata m
                parseLayout (fromMaybe 0 $ lookup "Water" metadata)
                            (fromMaybe 0 $ lookup "Flooding" metadata)
                            (fromMaybe 10 $ lookup "Waterproof" metadata)
                            l
  where (l, m) = break (=="") $ lines s

parseInt :: String -> MapParser Int
parseInt s = case reads $ takeWhile (not . isSpace) s of
               (x, ""):_ -> return x
               _ -> fail $ "'" ++ s ++ "' is not an integer"

parseMetadata :: [String] -> MapParser [(String, Int)]
parseMetadata = mapM parse . drop 1
  where parse s | "Waterproof" `isPrefixOf` s = parsenum "Waterproof" s
                | "Water" `isPrefixOf` s = parsenum "Water" s
                | "Flooding" `isPrefixOf` s = parsenum "Flooding" s
                | otherwise = fail $ "Invalid metadata: " ++ s
        parsenum k s = do x <- parseInt (drop 1 $ dropWhile (not . isSpace) s)
                          return (k, x)

parseLayout :: Int -> Int -> Int -> [String] -> MapParser MineMap
parseLayout w1 f w2 s = mk s
  where mk = mkMap <=< foldM combine (Nothing, newMap (w,h) w1 f w2)
             . concatMap mkpos . zip [1..] . reverse
        (w, h) = (maximum (map length s), length s)
        mkpos (y,l) = zipWith (\x c -> ((x,y), c)) [1..] l
        combine (rp, m) (p, c) = do
          c' <- parseChar c
          let m' = setCell m p c'
          case c' of
            Robot | isJust rp -> fail "Two robots in map"
                  | otherwise -> return (Just p, m')
            _ -> return (rp, m')
        mkMap (Nothing, _) = Left "No robot in map"
        mkMap (Just _, m) = Right m
