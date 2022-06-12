{-# LANGUAGE DuplicateRecordFields #-}
module Module2 where

import Module1

data R2 = MkR2 { x :: Int }

update r = r { Module1.x = 1 }