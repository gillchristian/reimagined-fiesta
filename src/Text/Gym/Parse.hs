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
import qualified Text.Parsec.String                   as P
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

set :: P.Parser Set
set = Comb.choice [P.try parseFullSet, P.try parseWithHelpSet, parsePartialSet]

sets :: P.Parser [Set]
sets = Comb.sepEndBy1 set C.space

-- "1x1x1.1"
-- "1x1x1"
parseFullSet :: P.Parser Set
parseFullSet =
  FullSet 
    <$> N.decimal <* C.char 'x'
    <*> N.decimal <* C.char 'x'
    <*> N.floating2 False

-- "1x1"
parsePartialSet :: P.Parser Set
parsePartialSet = 
  SetZero
    <$> N.decimal <* C.char 'x'
    <*> N.decimal

-- "1x1xh1.1"
-- "1x1xh1"
parseWithHelpSet :: P.Parser Set
parseWithHelpSet =
  SetWithHelp 
    <$> N.decimal <* C.char 'x'
    <*> N.decimal <* C.char 'x' <* C.char 'h'
    <*> N.floating2 True
