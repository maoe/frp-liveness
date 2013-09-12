{-# LANGUAGE TupleSections #-}
module FRP where
import Control.Applicative (Applicative(..))
import Data.Monoid ((<>))

import Clock hiding (tail)
import qualified Clock as C
import Set (Set, Relation)
import qualified Set as S

data Behaviour t a = Behaviour (Clock t) (Time -> Set a)

instance Functor (Behaviour t) where
    fmap g (Behaviour c f) = Behaviour c (fmap g . f)

data Event t a = Event (Clock t) (Relation Time a)

instance Functor (Event t) where
    fmap f (Event c r) = Event c (fmap (fmap f) r)

event :: Behaviour t a -> Event t a
event (Behaviour c f) = Event c (S.relation (times c) f)

buffer :: Event t a -> Behaviour t a
buffer (Event c r) = Behaviour c (S.buffer r)

delay :: Behaviour t a -> Event (Tail t) a
delay (Behaviour c f) = Event (C.tail c) (S.pmapr f (C.prev c))

cons :: a -> Event (Tail t) a -> Event t a
cons x (Event c r) = Event c' (r <> r')
  where
    c' = tailInv c
    r' = fmap (, x) (first c') -- XXX

fix :: Clock t -> (Event (Tail t) a -> Behaviour t a) -> Behaviour t a
fix c f = s where s = f (delay s)

constant :: Clock t -> a -> Behaviour t a
constant c x = fix c (\xs -> buffer (cons x xs))

union :: Event s a -> Event (Minus t s) a -> Event t a
union (Event _ q) (Event c r) = Event (minusInv c) (q <> r)
