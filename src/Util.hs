module Util where

import Simulation
import MineMap

import qualified Data.Set as S


liftIsBlocked :: SimState -> Bool
liftIsBlocked s = not $ any passable $ map (getCell mm) fields
  where
    passable (Target _) = True  -- TODO: Check trampoline
    passable f = f `elem` [Earth, Robot, Wall, Lambda, Empty]
    fields = filter (isValidPos mm) $
             map (move pos) [MoveLeft, MoveRight, MoveUp, MoveDown]

    mm = mineMap s
    pos = robot mm