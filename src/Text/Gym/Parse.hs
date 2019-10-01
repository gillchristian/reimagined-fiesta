module Text.Gym.Parse
  ( Sets
  , Reps
  , Weight
  , Set(..)
  , Exercise(..)
  , Routine(..)
  , fileParser
  ) where

import           Data.Functor                         (($>))
import qualified Text.Parsec.Char                     as C
import qualified Text.Parsec.Combinator               as Comb
import           Text.Parsec.Prim                     ((<?>), (<|>))
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

data Exercise =
  Exercise String
           [Set]
           (Maybe String)
  deriving (Show, Eq)

data Routine =
  Routine String
          [Exercise]
  deriving (Show, Eq)

fileParser :: P.Parser [Routine]
fileParser = Comb.many1 routine

-- consumes the extra new line (two \n between routines)
routine :: P.Parser Routine
routine = Routine <$> label <*> exercises
  where
    label = C.char '#' *> C.spaces *> tilEol <* C.spaces
    exercises = Comb.manyTill exercise (eol <|> Comb.eof)

exercise :: P.Parser Exercise
exercise = Exercise <$> label <*> sets <*> commentOrNewLine
  where
    sets = Comb.sepEndBy1 set (Comb.many1 $ C.char ' ')
    label =
      (Comb.many1 (C.noneOf ":\n") <* C.char ':' <* C.spaces) <?>
      "\":\" separating exercise label from sets"
    -- consumes the new line
    commentOrNewLine = P.try comment <|> C.newline $> Nothing

comment :: P.Parser (Maybe String)
comment =
  Comb.optionMaybe
    (C.spaces *> C.string "//" *> C.spaces *> Comb.manyTill C.anyChar C.newline)

set :: P.Parser Set
set = (P.try fullSet <|> P.try withHelpSet <|> partialSet) <?> "set"

-- "1x1x1.1"
-- "1x1x1"
fullSet :: P.Parser Set
fullSet =
  FullSet <$> N.decimal <* C.char 'x' <*> N.decimal <* C.char 'x' <*>
  N.floating2 False <?> "full set (e.g. \"1x1x1\")"

-- "1x1"
partialSet :: P.Parser Set
partialSet =
  SetZero <$> N.decimal <* C.char 'x' <*>
  N.decimal <?> "partial set (e.g. \"1x1\")"

-- "1x1xh1.1"
-- "1x1xh1"
withHelpSet :: P.Parser Set
withHelpSet =
  SetWithHelp <$> N.decimal <* C.char 'x' <*> N.decimal <* C.char 'x' <*
  C.char 'h' <*>
  N.floating2 True <?> "set with help (e.g. \"1x1\")"

tilEol :: P.Parser String
tilEol = Comb.many1 $ C.noneOf "\r\n"

eol :: P.Parser ()
eol = C.newline $> ()
