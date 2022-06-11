{-# LANGUAGE TypeApplications, RankNTypes #-}

module MissingSpaceTypeApplication where

f :: (forall a. a -> b) -> (Int -> b)
f x = x @Int
