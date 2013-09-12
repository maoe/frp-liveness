{-# LANGUAGE TupleSections #-}
module Set where
import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Data.Monoid (Monoid(..))
import Prelude hiding (filter)
import qualified Data.Function as F (fix)

import Continuation (Not)
import qualified Continuation as C

-----------------------------------------------------------

data Set a = Set { unSet :: Not (Not a) }

instance Monoid (Set a) where
    mempty = Set C.discard
    mappend (Set xs) (Set ys) = Set (xs `C.andThen` ys)

instance Functor Set where
    fmap f xs = xs >>= return . f

instance Applicative Set where
    pure = return
    (<*>) = ap

instance Monad Set where
    return = Set . C.doubleNot
    Set xs >>= f = Set (C.comap (C.copmap (unSet . f)) xs)

fix :: ((a -> Set b) -> a -> Set b) -> a -> Set b
fix f = Set . F.fix (\g -> unSet . f (Set . g))

filter :: (a -> Bool) -> Set a -> Set a
filter f xs = do
    x <- xs
    if f x then return x else mempty

-----------------------------------------------------------

type Relation a b = Set (a, b)

relation :: Set a -> (a -> Set b) -> Relation a b
relation xs f = xs >>= (\x -> fmap (x,) (f x))

buffer :: Eq a => Relation a b -> a -> Set b
buffer rel = Set . C.buffer (unSet rel)

pmapr :: (b -> Set c) -> Relation a b -> Relation a c
pmapr f rel = rel >>= (\(x, y) -> fmap (x,) (f y))

domain :: Relation a b -> Set a
domain = fmap fst

diagonal :: Set a -> Relation a a
diagonal = fmap (\x -> (x, x))

comp :: Eq b => Relation a b -> Relation b c -> Relation a c
comp q r = pmapr (buffer r) q
