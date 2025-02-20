module Jelly.Sub
 ( trySubstitute
 ) where

import Prelude

import Data.String (stripSuffix, length)
import Data.String.Pattern (Pattern(..))
import Data.Maybe (Maybe(..), maybe')
import Data.Functor (class Functor)
import Control.Alternative (class Alt, class Plus, class Alternative, (<|>), empty)
import Data.Newtype (class Newtype, wrap, unwrap)
import Control.Monad.Writer (Writer, execWriter, tell)

type SingleSub = {prefix :: String, sub :: String, deltaLength :: Int}

newtype Substitution a b = Substitution (a -> Maybe b)
derive instance Newtype (Substitution a b) _

infixr 0 type Substitution as ->?

instance Functor (Substitution a) where
  map f = wrap <<< map f <<< unwrap -- I have NO idea why modify <<< map doesn't typecheck. ???

instance Alt (Substitution a) where
  alt a b = wrap \x -> -- Not using Alt Maybe because no laziness means no short circuting!!
    maybe' (\_ -> unwrap b x) $ unwrap a x

instance Plus (Substitution a) where
  empty = wrap $ const Nothing

instance Alternative (Substitution a)

instance Semigroup (Substitution a b) where
  append = (<|>)

instance Monoid (Substitution a b) where
  mempty = empty

makeSubstitution :: String -> String -> String ->? SingleSub
makeSubstitution pat to = wrap \text -> do
  prefix <- stripSuffix $ Pattern pat
  pure {
    prefix,
    sub: to,
    deltaLength: length to - length pat
  }

-- we love abusable notation
makeTellSubstitution :: String -> String -> Writer (String ->? SingleSub) Unit
makeTellSubstitution = (tell <<< _) <<< makeSubstitution

infix 5 makeTellSubstitution as ~>

-- THE entry point to the impure frontend
trySubstitute :: String -> Maybe String
trySubstitute = unwrap $ tryBatchSubstitute <|> trySingleSubstitutions

tryBatchSubstitute :: String ->? String
tryBatchSubstitute = wrap $ const Nothing -- TODO

trySingleSubstitutions :: String ->? String
trySingleSubstitutions = execWriter do
  "!mentoscola" ~> "うそだろおい…"
