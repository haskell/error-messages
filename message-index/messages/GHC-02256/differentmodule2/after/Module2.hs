{-# LANGUAGE DuplicateRecordFields #-}
module Module2 where

import Module1 (R1)
import qualified Module1 as R1

data R2 = MkR2 { x :: Int }

update :: R1 -> R1
update r = r { R1.x = 1 }