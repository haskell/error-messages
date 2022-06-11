{-# LANGUAGE TypeApplications, RankNTypes #-}

module MissingSpaceTypeOperator where

f :: (forall a. a -> b) -> (Int -> b)
f x = x@Int
