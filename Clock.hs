module Clock where
import Prelude hiding (tail)
import Set (Set, Relation)
import qualified Set as S

type Time = Integer

data Clock t = Clock
  { times :: Set Time
  , first :: Set Time
  , prev :: Relation Time Time
  , parent :: t
  }

newtype Tail t = Tail { tailClock :: Clock t }

tail :: Clock t -> Clock (Tail t)
tail c = Clock
  { times = S.domain (prev c)
  , first = do
      t <- first c
      S.domain $ S.filter (\(_, u) -> t == u) (prev c)
  , prev = do
      t <- first c
      S.filter (\(_, u) -> t < u) (prev c)
  , parent = Tail c -- XXX
  }

tailInv :: Clock (Tail t) -> Clock t
tailInv = tailClock . parent

data Minus t s -- TODO

minusInv :: Clock (Minus t s) -> Clock t
minusInv = undefined -- TODO
