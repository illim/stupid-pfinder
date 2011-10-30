{-# LANGUAGE TypeSynonymInstances #-}

module Models where

import Data.Map

type Point = (Int, Int)

newtype Position = Position Point

type Entity = Position

data Field = Field Int (Map Point Int)

data World = World Field [Entity]


isClean :: World -> Point -> Bool
isClean (World (Field size _) entities) (x, y) =
    (x > -1
     && x < size
     && y > -1
     && y < size
     && (not $ any (\(Position (a, b)) -> a == x && b == y) entities) )

class Addable a where
  add :: a -> a -> a

instance Addable Point where
  add (a, b) (c, d) = (a +c, b+d)
