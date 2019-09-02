module Text.Gym.Parse
  ( Set(..)
  , set
  , sets
  ) where

import qualified Data.ByteString.Char8                as Bs
import qualified Data.Functor.Identity                as FI
import qualified Text.Parsec.Char                     as C
import qualified Text.Parsec.Combinator               as Comb
import qualified Text.Parsec.Prim                     as P
import qualified Text.ParserCombinators.Parsec.Number as N

type Sets = Int

type Reps = Int

type Weight = Float

data Set
  = FullSet Sets
            Reps
            Weight
  | SetZero Sets
            Reps
  | SetWithHelp Sets
                Reps
                Weight
  deriving (Show, Eq)

-- TODO `FI.Identity` ?
type ParsecT a r = P.ParsecT String a FI.Identity r

set :: ParsecT a Set
set =
  Comb.choice
    [P.try parseFullSet, P.try parseWithHelpSet, parsePartialSet]

sets :: ParsecT a [Set]
sets = Comb.sepEndBy1 set C.space

-- "1x1x1.0"
parseFullSet :: ParsecT a Set
parseFullSet =
  FullSet <$> N.decimal <* C.char 'x'
          <*> N.decimal <* C.char 'x'
          <*> N.floating2 False

-- TODO: this should be used if `parseFullSet` didn't work
parsePartialSet :: ParsecT a Set
parsePartialSet = SetZero <$> N.decimal <* C.char 'x' <*> N.decimal

parseWithHelpSet :: ParsecT a Set
parseWithHelpSet =
  SetWithHelp <$> N.decimal <* C.char 'x'
              <*> N.decimal <* C.char 'x' <* C.char 'h'
              <*> N.floating2 True
