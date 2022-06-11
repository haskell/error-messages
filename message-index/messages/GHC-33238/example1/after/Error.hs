{-# LANGUAGE DuplicateRecordFields #-}
module Error where

data R1 = MkR1 { x :: Int, y :: Int }

data R2 = MkR2 { y :: Int, z :: Int }

update r = r { x = 1, y = 2 }