{-# LANGUAGE DuplicateRecordFields #-}
module Error where

data R1 = MkR1 { x :: Int }

data R2 = MkR2 { x :: Int }

update r = case r of MkR1 x -> MkR1 1