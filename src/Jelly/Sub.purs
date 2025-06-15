module Jelly.Sub
  ( trySubstitute
  ) where

import Prelude

import Data.String (stripSuffix, length, toLower, take)
import Data.String.Pattern (Pattern(..))
import Data.Maybe (Maybe(..), maybe')
import Control.Alternative (class Alt, class Plus, class Alternative, (<|>), empty)
import Data.Newtype (class Newtype, wrap, unwrap, modify)
import Control.Monad.Writer (Writer, execWriter, tell)

import Jelly.Codepage (Jel(..), BuiltinForm(..))

type SingleSub = {prefix :: String, sub :: String, deltaLength :: Int}

smoosh :: SingleSub -> String
smoosh s = s.prefix <> s.sub

type Alias b a = {metadata :: a, try :: b}

newtype TryAlias a b = TryAlias (a -> Maybe b)
derive instance Newtype (TryAlias a b) _

infixr 0 type Alias as :::
infixr 1 type TryAlias as ->?

instance Functor (TryAlias a) where
  map = (wrap <<< _) <<< (_ <<< unwrap) <<< map <<< map

instance Apply (TryAlias a) where
  apply f a = wrap \x -> unwrap f x <*> unwrap a x

instance Applicative (TryAlias a) where
  pure = wrap <<< const <<< pure

instance Alt (TryAlias a) where
  alt a b = wrap \x -> -- Not using Alt Maybe because no laziness means no short circuiting!!
    maybe' (\_ -> unwrap b x) pure $ unwrap a x

instance Plus (TryAlias a) where
  empty = wrap $ const Nothing

instance Alternative (TryAlias a)

instance Semigroup (TryAlias a b) where
  append = (<|>)

instance Monoid (TryAlias a b) where
  mempty = empty

makeAlias :: String -> String -> String ->? SingleSub
makeAlias pat to = wrap \text -> do
  prefix <- stripSuffix (Pattern pat) text
  pure {
    prefix,
    sub: to,
    deltaLength: length to - length pat -- uhhhhh wait am I even going to use this
  }

makeCaseInsensitiveAlias :: String -> String -> String ->? SingleSub
makeCaseInsensitiveAlias pat to = wrap \text -> do
  foldedPrefix <- stripSuffix (Pattern pat) $ toLower text
  pure
    { prefix: take (length foldedPrefix) text
    , sub: to
    , deltaLength: length to - length pat
    }

-- we love abusable notation
makeTellAlias :: forall a. Show a => String -> a -> Writer (String ->? SingleSub) Unit
makeTellAlias = (tell <<< _) <<< (_ <<< show) <<< makeAlias

makeTellCaseInsensitiveAlias :: forall a. Show a => String -> a -> Writer (String ->? SingleSub) Unit
makeTellCaseInsensitiveAlias = (tell <<< _) <<< (_ <<< show) <<< makeCaseInsensitiveAlias

makeTellStringAlias :: String -> String -> Writer (String ->? SingleSub) Unit
makeTellStringAlias = (tell <<< _) <<<  makeAlias

infix 5 makeTellAlias as ~>
infix 5 makeTellCaseInsensitiveAlias as ~~>
infix 5 makeTellStringAlias as :~>

-- THE entry point to the impure frontend
trySubstitute :: String -> Maybe String
trySubstitute = unwrap $ tryBatchSubstitute <|> trySingleAliases

tryBatchSubstitute :: String ->? String
tryBatchSubstitute = empty -- TODO

trySingleAliases :: String ->? String
trySingleAliases = smoosh <$> execWriter do
  "!mentoscola" :~> "うそだろおい…"
  ".repr" ~~> O1 BigROverdot -- TODO: put verbose aliases in their own block for organization
  ".U" ~> BigUUnderdot -- TODO: put conventional double char aliases in their own block, and then make a separate block for innovated aliases
