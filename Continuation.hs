module Continuation where

type Not a = a -> Bool

buffer :: Eq a => Not (Not (a, b)) -> a -> Not (Not b)
buffer k x l = k (\(x', y) -> x == x' && l y)

discard :: Not a
discard x = False

doubleNot :: a -> Not (Not a)
doubleNot x k = k x

andThen :: Not a -> Not a -> Not a
andThen k l x = k x || l x

comap :: (b -> a) -> Not a -> Not b
comap f = copmap (doubleNot . f)

copmap :: (b -> Not (Not a)) -> Not a -> Not b
copmap f k y = f y k
