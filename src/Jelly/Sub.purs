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

newtype Substitution a b = Substitution (a -> Maybe b)
derive instance Newtype (Substitution a b) _

infixr 0 type Substitution as ->?

instance Functor (Substitution a) where
  map f = (wrap <<< _) $ (_ <<< unwrap) $ map $ map f -- modify. still doesn't typecheck. modify is LITERALLY modify fn t = wrap (fn (unwrap t)) HOW IS THAT NOT. WHAT. modify and (wrap <<< _) <<< (_ <<< unwrap) literally have reconcilable types in spago repl and ????? there's no way the added flexibility matters here when it.s it's THE SAME NEWTYPE AAAAAAAAGFLAEMGL:ERMGLG:AFRG

instance Apply (Substitution a) where
  apply f a = wrap \x -> unwrap f x <*> unwrap a x 

instance Applicative (Substitution a) where
  pure = wrap <<< const <<< pure

instance Alt (Substitution a) where
  alt a b = wrap \x -> -- Not using Alt Maybe because no laziness means no short circuiting!!
    maybe' (\_ -> unwrap b x) pure $ unwrap a x

instance Plus (Substitution a) where
  empty = wrap $ const Nothing

instance Alternative (Substitution a)

instance Semigroup (Substitution a b) where
  append = (<|>)

instance Monoid (Substitution a b) where
  mempty = empty

makeSubstitution :: String -> String -> String ->? SingleSub
makeSubstitution pat to = wrap \text -> do
  prefix <- stripSuffix (Pattern pat) text
  pure {
    prefix,
    sub: to,
    deltaLength: length to - length pat -- uhhhhh wait am I even going to use this
  }

makeCaseInsensitiveSubstitution :: String -> String -> String ->? SingleSub
makeCaseInsensitiveSubstitution pat to = wrap \text -> do
  foldedPrefix <- stripSuffix (Pattern pat) $ toLower text
  pure {
    prefix: take (length foldedPrefix) text,
    sub: to,
    deltaLength: length to - length pat
  }

-- we love abusable notation
makeTellSubstitution :: forall a. Show a => String -> a -> Writer (String ->? SingleSub) Unit
makeTellSubstitution = (tell <<< _) <<< (_ <<< show) <<< makeSubstitution

makeTellCaseInsensitiveSubstitution :: forall a. Show a => String -> a -> Writer (String ->? SingleSub) Unit
makeTellCaseInsensitiveSubstitution = (tell <<< _) <<< (_ <<< show) <<< makeCaseInsensitiveSubstitution

makeTellStringSubstitution :: String -> String -> Writer (String ->? SingleSub) Unit
makeTellStringSubstitution = (tell <<< _) <<<  makeSubstitution

infix 5 makeTellSubstitution as ~>
infix 5 makeTellCaseInsensitiveSubstitution as ~~>
infix 5 makeTellStringSubstitution as :~>

-- THE entry point to the impure frontend
trySubstitute :: String -> Maybe String
trySubstitute = unwrap $ tryBatchSubstitute <|> trySingleSubstitutions

tryBatchSubstitute :: String ->? String
tryBatchSubstitute = empty -- TODO

trySingleSubstitutions :: String ->? String
trySingleSubstitutions = smoosh <$> execWriter do
  "!mentoscola" :~> "うそだろおい…"
  ".repr" ~~> ChunkyOMonad BigROverdot -- TODO: put verbose aliases in their own block for organization
  ".U" ~> BigUUnderdot -- TODO: put conventional double char aliases in their own block, and then make a separate block for innovated aliases
