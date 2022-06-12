{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wambiguous-fields #-}
module Error where

data R1 = MkR1 { x :: Int }

data R2 = MkR2 { y :: Int }

update :: R1 -> R1
update r = r { x = 1 }