module Jelly.Builtins
  ( Builtin(..)
  , Builtin'
  , BuiltinType(..)
  , Adicity(..)
  , QuickArg(..)
  , QuickArgsFormatter
  , formatCompact
  , formatVerbose
  , builtin
  , stringTerminator
  ) where

import Prelude
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..))
import Jelly.Codepage (Jel(..), BuiltinForm(..))
import Type.Markdown (Markdown, md)

data Builtin = Builtin BuiltinType Builtin'

type Builtin' =
  { mnemonic :: String
  , keywords :: Array String
  , originalDescription :: Markdown
  , revisedDescription :: Markdown
  }

data BuiltinType
  = Atom Adicity
  | Quick (Array QuickArg)
  | Syntax

data Adicity = Niladic | Monadic | Dyadic

adicSuffix :: Adicity -> String
adicSuffix Niladic = "0"
adicSuffix Monadic = "1"
adicSuffix Dyadic = "2"

adicNoun :: Adicity -> String
adicNoun Niladic = "nilad"
adicNoun Monadic = "monad"
adicNoun Dyadic = "dyad"

data QuickArg
  = Q String
  | OptionalNilad String
  | SemiOptional String
  | Varargs String

type QuickArgsFormatter a = forall f. Foldable f => f QuickArg -> BuiltinForm -> a

-- yeah whatever this may as well live here for now at least
formatCompact :: QuickArgsFormatter String
formatCompact args quick = foldMap format' args <> show quick
  where
  format' :: QuickArg -> String
  format' (Q s) = s <> " "
  format' (OptionalNilad s) = s <> "?? "
  format' (SemiOptional s) = s <> "? "
  format' (Varargs s) = s <> "+ "

-- TODO: make this Markdown (later!!!)
formatVerbose :: QuickArgsFormatter String
formatVerbose args quick = foldMap format' args <> show quick
  where
  format' :: QuickArg -> String
  format' (Q s) = "__" <> s <> "__ "
  format' (OptionalNilad s) = "[__" <> s <> "__ *only if niladic*] "
  format' (SemiOptional s) = "[__" <> s <> "__ *or last program argument*] "
  format' (Varargs s) = "__" <> s <> "__... "

fibLoop :: Markdown -> Markdown
fibLoop desc = desc <> (md @" If dyadic, the right argument to each subsequent iteration is the left argument to the previous iteration.")

quickchainLCC :: Int -> String -> Adicity -> Builtin
quickchainLCC n n' a = Builtin (Quick [Varargs "links"])
  { mnemonic: "group" <> show n <> adicSuffix a
  , keywords: ["group", "chain"]
  , originalDescription: -- TODO: wrap this in Markdown when that's actually different ig
    "Last " <> n' <> " links (if not part of an LCC) as a " <> adicNoun a <> "."
  , revisedDescription:
    "Group at least " <> n' <> " links into a " <> adicNoun a <> ", consuming more for every nilad followed by a monad or by dyad-nilad pairs."
  }

builtin :: BuiltinForm -> Maybe Builtin

-- SINGLE BYTE NILADS --

-- SINGLE BYTE MONADS --

-- SINGLE BYTE DYADS --

-- ASSORTED NILADS --

-- ARITHMETIC MONADS --

-- ARITHMETIC DYADS --

-- OTHER MONADS --

-- OTHER DYADS --

-- QUICKS --

builtin (Single Copyright) = Just $ Builtin (Quick [Q "link"])
  { mnemonic: "copy"
  , keywords: []
  , originalDescription: md @
    "Copy link result to register (`®` atom to retrieve)."
  , revisedDescription: md @
    "Copy `link`'s result to the register, returning it unchanged."
  }
builtin (Single Eszett) = Just $ Builtin (Quick [])
  { mnemonic: "recur"
  , keywords: ["recur", "furlong"]
  , originalDescription: md @
    "This link, with the same arity."
  , revisedDescription: md @
    "Recursively invoke the current furlong, with adicity `-1`."
  }
builtin (Single Cent) = Just $ Builtin (Quick [])
  { mnemonic: "helper0"
  , keywords: ["last", "constant", "furlong"]
  , originalDescription: md @
    "Last link as a nilad."
  , revisedDescription: md @
    "Invoke the furlong above this one, as a nilad."
  }
builtin (Single BigCCedilla) = Just $ Builtin (Quick [])
  { mnemonic: "helper1"
  , keywords: ["last", "helper", "furlong"]
  , originalDescription: md @
    "Last link as a monad."
  , revisedDescription: md @
    "Invoke the furlong above this one, as a monad."
  }
builtin (Single LittleCCedilla) = Just $ Builtin (Quick [])
  { mnemonic: "helper2"
  , keywords: ["last", "helper", "furlong"]
  , originalDescription: md @
    "Last link as a dyad."
  , revisedDescription: md @
    "Invoke the furlong above this one, as a dyad."
  }
builtin (Single BigEnye) = Just $ Builtin (Quick [])
  { mnemonic: "next1"
  , keywords: ["next", "helper", "furlong"]
  , originalDescription: md @
    "Next link as a monad."
  , revisedDescription: md @
    "Invoke the furlong below this one, as a monad."
  }
builtin (Single LittleEnye) = Just $ Builtin (Quick [])
  { mnemonic: "next2"
  , keywords: ["next", "helper", "furlong"]
  , originalDescription: md @
    "Next link as a dyad."
  , revisedDescription: md @
    "Invoke the furlong below this one, as a dyad."
  }
builtin (Single Pound) = Just $ Builtin (Quick [Q "n"])
  { mnemonic: "fAt0"
  , keywords: ["index", "at", "constant", "furlong"]
  , originalDescription: md @
    "Link at index n as a nilad."
  , revisedDescription: md @
    "Invoke the (non-main) furlong at index `n` from the top, as a nilad."
  }
builtin (Single BigLOverdot) = Just $ Builtin (Quick [Q "n"])
  { mnemonic: "fAt1"
  , keywords: ["index", "at", "helper", "furlong"]
  , originalDescription: md @
    "Link at index n as a monad."
  , revisedDescription: md @
    "Invoke the (non-main) furlong at index `n` from the top, as a monad."
  }
builtin (Single LittleLOverdot) = Just $ Builtin (Quick [Q "n"])
  { mnemonic: "fAt2"
  , keywords: ["index", "at", "helper", "furlong"]
  , originalDescription: md @
    "Link at index n as a dyad."
  , revisedDescription: md @
    "Invoke the (non-main) furlong at index `n` from the top, as a dyad."
  }
builtin (Single BrokenBar) = Just $ Builtin (Quick [Q "link", Q "indices"])
  { mnemonic: "sparse"
  , keywords: ["sparse", "mask", "select"]
  , originalDescription: md @
    "Apply link to items at specific indices."
  , revisedDescription: md @
    "Apply `link`, then mask resulting items into original left argument at `indices`."
  }
builtin (Single Lcxe) = Just $ Builtin (Quick [Q "link", SemiOptional "repetitions"])
  { mnemonic: "repeat"
  , keywords: ["repeat", "loop", "iterate"]
  , originalDescription: md @
    "Repeat n times."
  , revisedDescription: fibLoop $ md @
    "Iteratively invoke `link` on its result `n` times."
  }
builtin (Single Euq) = Just $ Builtin (Quick [Q "body", Q "condition"])
  { mnemonic: "while"
  , keywords: ["while", "repeat", "loop", "iterate", "conditional"]
  , originalDescription: md @
    "While loop."
  , revisedDescription: fibLoop $ md @
    "Iteratively invoke `body` on its result while `condition` holds on the current arguments."
  }
builtin (Single Slash) = Just $ Builtin (Quick [Q "dyad", OptionalNilad "n"])
  { mnemonic: "reduce"
  , keywords: ["reduce", "fold", "foldl1", "for", "windows"]
  , originalDescription: md @
    "Reduce or n-wise reduce."
  , revisedDescription: md @
    "Left-associative fold by `dyad`. Fold over non-overlapping windows if `n` given. Always monadic."
  }
builtin (Single LittleFHook) = Just $ Builtin (Quick [Q "dyad", OptionalNilad "n"])
  { mnemonic: "fold"
  , keywords: ["reduce", "fold", "foldl", "for", "windows"]
  , originalDescription: md @
    "Reduce or n-wise reduce using the right argument as the starting value."
  , revisedDescription: md @
    "Left-associative fold by `dyad`, starting with the right argument. Fold over non-overlapping windows if `n` given."
  }
builtin (Single Backslash) = Just $ Builtin (Quick [Q "dyad", OptionalNilad "n"])
  { mnemonic: "scan"
  , keywords: ["scan", "reduce", "cumulative", "scanl1", "for", "windows"]
  , originalDescription: md @
    "Cumulative reduce or n-wise overlapping reduce."
  , revisedDescription: md @
    "Left-associative scan by `dyad`. Fold over overlapping windows instead if `n` given. Always monadic."
  }
builtin (Single Currency) = Just $ Builtin (Quick [Varargs "links"])
  { mnemonic: "group0"
  , keywords: ["group", "chain", "nilad", "constant"]
  , originalDescription: md @
    "Nilad followed by links as a nilad."
  , revisedDescription: md @
    "Group at least two links into a (weak/coercible) nilad, consuming links until a nilad is found."
  }
builtin (Single Dollar) = Just $
  quickchainLCC 2 "two" Monadic
builtin (Single BigDHook) = Just $
  quickchainLCC 3 "three" Monadic
builtin (Single BigLabiodentalApproximant) = Just $
  quickchainLCC 4 "four" Monadic
builtin (Single Yen) = Just $
  quickchainLCC 2 "two" Dyadic
builtin (Single LittleDHook) = Just $
  quickchainLCC 3 "three" Dyadic
builtin (Single LittleLabiodentalApproximant) = Just $
  quickchainLCC 4 "four" Dyadic
builtin (Single Hash) = Just $ Builtin (Quick [Q "condition", SemiOptional "amount"])
  { mnemonic: "nfind"
  , keywords: ["brute", "force", "first", "find"]
  , originalDescription: md @
    -- ...do I ACTUALLY want to preserve the hyperlink :p
    "[`nfind`](https://github.com/DennisMitchell/jelly/blob/dd231009e232e231b851bc360912d91e100b4515/jelly.py#L349): Count up, collecting first n matches."
  , revisedDescription: md @
    "Counting up from the left argument (or 0 if invoked niladically), collect `amount` values for which `condition` holds."
  }
builtin (Single Que) = Just $ Builtin (Quick [Q "if-clause", Q "else-clause", Q "condition"])
  { mnemonic: "if"
  , keywords: ["if", "else", "conditional", "ternary"]
  , originalDescription: md @
    "Ternary if."
  , revisedDescription: md @
    "Evaluate `condition`, then evaluate `if-clause` if truthy or `else-clause` if not."
  }
builtin (Single BigFHook) = Just $ Builtin (Quick [Q "link"])
  { mnemonic: "invariant"
  , keywords: ["invariant", "unchanged", "equal"]
  , originalDescription: md @
    "Check if the left argument is equal to the result."
  , revisedDescription: md @
    "Test whether or not the result of `link` is equal to the left argument. Always returns a boolean scalar."
  }
builtin (Single SuperPlus) = Just $ Builtin (Quick [Q "link"])
  { mnemonic: "duplicate"
  , keywords: ["duplicate", "repeat", "copy"]
  , originalDescription: md @
    "Duplicates the previous link."
  , revisedDescription: md @
    "Push `link` twice to the chain. Does *not* peek; will pop and subordinate a previous chain if needed."
  }
builtin (Single At) = Just $ Builtin (Quick [Q "dyad"])
  { mnemonic: "swap"
  , keywords: ["swap", "commute", "mirror", "switch", "reverse"]
  , originalDescription: md @
    "Swaps operands."
  , revisedDescription: md @
    "Evaluate `dyad` with the right argument on the left and vice versa."
  }
builtin (Single At) = Just $ Builtin (Quick [Q "dyad"])
  { mnemonic: "self"
  , keywords: ["self", "selfie", "repeat"]
  , originalDescription: md @
    "Make a monad from a dyad by repeating the argument."
  , revisedDescription: md @
    "Evaluate `dyad` with the monadic argument on the left and right."
  }
builtin (Single DoubleQuote) = Just $ Builtin (Quick [Q "dyad"])
  { mnemonic: "zipwith"
  , keywords: ["zipwith", "zip", "corresponding", "pairs"]
  , originalDescription: md @
    "Make a monad from a dyad by repeating the argument."
  , revisedDescription: md @
    "Evaluate `dyad` with the monadic argument on the left and right."
  }
builtin (Single SingleQuote) = Just $ Builtin (Quick [Q "link"])
  { mnemonic: "flat"
  , keywords: ["flat", "spawn", "table", "jank"]
  , originalDescription: md @
    "For monads, flat. For dyad, spawn."
  , revisedDescription: md @
    "Attempt to suppress vectorization for `link`. Use at your own risk."
  }
builtin (Single LittleThorn) = Just $ Builtin (Quick [Q "dyad"])
  { mnemonic: "table"
  , keywords: ["table", "product", "outer", "cartesian", "each", "pairs"]
  , originalDescription: md @
    "Outer product/table."
  , revisedDescription: md @
    "Map `dyad` over the left argument, mapped over the right argument."
  }
builtin (Single OpenBrace) = Just $ Builtin (Quick [Q "monad"])
  { mnemonic: "ofLeft"
  , keywords: ["left", "const", "ignore"]
  , originalDescription: md @
    "Turn a monad into a dyad. Uses the left argument."
  , revisedDescription: md @
    "Turn a monad into a dyad which ignores the right argument."
  }
builtin (Single OpenBrace) = Just $ Builtin (Quick [Q "monad"])
  { mnemonic: "ofRight"
  , keywords: ["right", "const", "ignore"]
  , originalDescription: md @
    "Turn a monad into a dyad. Uses the right argument."
  , revisedDescription: md @
    "Turn a monad into a dyad which ignores the left argument."
  }
builtin (Single Euro) = Just $ Builtin (Quick [Q "link"])
  { mnemonic: "each"
  , keywords: ["each", "map", "for"]
  , originalDescription: md @
    "Each. Map a link over its left argument."
  , revisedDescription: md @
    "Map `link` over the left argument."
  }
builtin (Single BigThorn) = Just $ Builtin (Quick [Q "link"])
  { mnemonic: "sortBy"
  , keywords: ["sort", "key", "grade"]
  , originalDescription: md @
    "Sort by some key function."
  , revisedDescription: md @
    "Sort the left argument by the result of `link` on each of its elements."
  }
builtin (Single BigNHook) = Just $ Builtin (Quick [Varargs "links"])
  { mnemonic: "neighbors"
  , keywords: ["neighbor", "pairs", "window", "adjacent"]
  , originalDescription: md @
    "Apply a dyadic link or a monadic chain for all pairs of neighboring elements."
  , revisedDescription: md @
    "Map `links` over overlapping pairs of adjacent elements, on the left and right for a single dyad, or as a list for a monadic chain (grouped with arbitrarily many trailing nilads)."
  }

-- SYNTAX --

builtin _ = Nothing

-- separate from builtin so I don't have to build that "is this also a terminator??"
-- into the builtin data itself lmao
stringTerminator :: Jel -> Maybe Builtin'
stringTerminator OpenGuillemet = Just
  { mnemonic: "termUnimpl"
  , keywords: ["string", "plain", "verbatim", "unimplemented", "dead"]
  , originalDescription: md @
    "[No original description -- not an intentional builtin]"
  , revisedDescription: md @
    "Terminate a plain string; equivalent to `”`."
  }
stringTerminator CloseGuillemet = Just
  { mnemonic: "termDict"
  , keywords: ["string", "dictionary", "compression"]
  , originalDescription: md @
    "Terminates a dictionary-compressed string."
  , revisedDescription: md @
    "Terminate a dictionary-compressed string."
  }
stringTerminator OpenSingleQuote = Just
  { mnemonic: "termOrd"
  , keywords: ["string", "integer", "ord", "codepoint", "byte", "sbcs"]
  , originalDescription: md @
    "Terminates a code-page index list. Jelly's version of `ord()`."
  , revisedDescription: md @
    "Terminate a string to be interpreted as a numeric list of Jelly SBCS codepoints."
  }
stringTerminator CloseSingleQuote = Just
  { mnemonic: "term250"
  , keywords: ["string", "integer", "base", "compression"]
  , originalDescription: md @
    "Terminates a base-250 number."
  , revisedDescription: md @
    "Terminate a base-250 number."
  }
stringTerminator OpenDoubleQuote = Just
  { mnemonic: "stringSep"
  , keywords: ["string", "list", "separator"]
  , originalDescription: md @
    "Begins a string literal, and separates a list of strings inside a string literal."
  , revisedDescription: md @
    "Separate elements of a list of strings within one literal."
  }
stringTerminator CloseDoubleQuote = Just
  { mnemonic: "termPlain"
  , keywords: ["string", "plain", "verbatim"]
  , originalDescription: md @
    "Terminates a regular string or a list of strings. Without `“`, a character literal."
  , revisedDescription: md @
    "Terminate a plain string."
  }
stringTerminator _ = Nothing
