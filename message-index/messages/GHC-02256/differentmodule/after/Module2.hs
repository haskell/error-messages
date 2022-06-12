{-# LANGUAGE DuplicateRecordFields #-}
module Module2 where

import Module1 ( R1(x) )

data R2 = MkR2 { x :: Int }

update :: R1 -> R1
update r = r { Module1.x = 1 }