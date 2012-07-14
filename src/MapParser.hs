module MapParser
  ( MapParser
  , parseMap)
  where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import MineMap

type MapParser = Either String

parseChar :: (Char -> MapParser Pos) -> Char -> MapParser Cell
parseChar _ ' ' = return Empty
parseChar _ '*' = return Rock
parseChar _ '#' = return Wall
parseChar _ '\\' = return Lambda
parseChar _ '.' = return Earth
parseChar _ 'L' = return $ Lift Closed
parseChar _ 'O' = return $ Lift Open
parseChar _ 'R' = return Robot
parseChar f c | c >= 'A' && c <= 'I' = Trampoline c <$> f c
              | c >= '1' && c <= '9' = return $ Target (read [c])
              | otherwise =  fail $ "Invalid character in map: '" ++ [c] ++ "'"

parseMap :: String -> MapParser MineMap
parseMap s = do meta <- parseMetadata $ filter (/="") m
                parseLayout meta l
  where (l, m) = break (=="") $ lines s

parseInt :: String -> MapParser Int
parseInt s = case reads $ takeWhile (not . isSpace) s of
               (x, ""):_ -> return x
               _ -> fail $ "'" ++ s ++ "' is not an integer"

data Metadata = Water Int | Flooding Int | Waterproof Int | TrampConnection Char Int

findWater :: [Metadata] -> Int
findWater = fromMaybe 0 . listToMaybe . mapMaybe wt
  where wt (Water w) = Just w
        wt _         = Nothing

findFlooding :: [Metadata] -> Int
findFlooding = fromMaybe 0 . listToMaybe . mapMaybe fl
  where fl (Flooding w) = Just w
        fl _            = Nothing

findWaterproof :: [Metadata] -> Int
findWaterproof = fromMaybe 10 . listToMaybe . mapMaybe wt
  where wt (Waterproof w) = Just w
        wt _              = Nothing

findTrampoline :: [Metadata] -> Char -> Maybe Int
findTrampoline meta c = listToMaybe $ mapMaybe tramp meta
  where tramp (TrampConnection c' k) | c == c' = Just k
        tramp _                                = Nothing

parseMetadata :: [String] -> MapParser [Metadata]
parseMetadata = mapM parse
  where parse s | "Waterproof" `isPrefixOf` s = parsenum Waterproof s
                | "Water" `isPrefixOf` s = parsenum Water s
                | "Flooding" `isPrefixOf` s = parsenum Flooding s
                | "Trampoline" `isPrefixOf` s =
                let s' = dropWhile isSpace $ dropWhile (not . isSpace) s
                in case s' of [] -> bad
                              src:s'' -> let s''' = dropWhile isSpace $ dropWhile (not . isSpace) $ dropWhile isSpace s''
                                         in liftM (TrampConnection src) $ parseInt s'''
                | otherwise = bad
                where bad = fail $ "Invalid metadata: " ++ s
        parsenum k s = liftM k $ parseInt (drop 1 $ dropWhile (not . isSpace) s)

trampDest :: [(Pos, Char)] -> Int -> MapParser Pos
trampDest s c = case find ((==head (show c)) . snd) s of
                  Nothing    -> fail $ "No destination for trampoline " ++ show c
                  Just (p,_) -> return p

parseLayout :: [Metadata] -> [String] -> MapParser MineMap
parseLayout meta s = do
  let td c = case findTrampoline meta c of
               Nothing -> fail $ "Trampoline " ++ [c] ++ " not defined"
               Just k  -> trampDest s' k
      combine (rp, m) (p, c) = do
        c' <- parseChar td c
        let m' = setCell m p c'
        case c' of
          Robot | isJust rp -> fail "Two robots in map"
                | otherwise -> return (Just p, m')
          _ -> return (rp, m')
  mkMap =<< foldM combine (Nothing, mp) s'
  where s' = concatMap mkpos $ zip [1..] $ reverse s
        mp = newMap (w,h) (findWater meta)
                          (findFlooding meta)
                          (findWaterproof meta)
        (w, h) = (maximum (map length s), length s)
        mkpos (y,l) = zipWith (\x c -> ((x,y), c)) [1..] l
        mkMap (Nothing, _) = Left "No robot in map"
        mkMap (Just _, m) = Right m
