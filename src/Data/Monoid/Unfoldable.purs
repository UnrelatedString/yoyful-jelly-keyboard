module Data.Monoid.Unfoldable
 ( TrivialUnfold

 ) where

import Prelude

import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, unwrap)

data Unfolder a b = Unfolder (b -> Maybe (a /\ b)) b
newtype TrivialUnfold a = TrivialUnfold (Exists (Unfolder a))
derive instance Newtype (TrivialUnfold a) _

instance Unfoldable TrivialUnfold where
  unfoldr f b = TrivialUnfold $ mkExists $ Unfolder f b

instance Unfoldable1 TrivialUnfold where
  unfoldr1 :: forall a b. (b -> a /\ Maybe b) -> b -> TrivialUnfold a
  unfoldr1 f = unfoldr adapter <<< Just
    where adapter :: Maybe b -> Maybe (a /\ Maybe b)
          adapter = map f
