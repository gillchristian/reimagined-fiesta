module Text.Gym.Parse
  ( Set(..)
  , set
  , exercise
  , routine
  , comment
  , file
  , tilEndOfLine
  ) where

import qualified Data.ByteString.Char8                as Bs
import qualified Data.Functor.Identity                as FI
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

file :: P.Parser [Routine]
file = Comb.many1 routine

routine :: P.Parser Routine
routine =
  do C.char '#'
     C.spaces
     date <- tilEndOfLine
     C.spaces
     exercises <- Comb.manyTill exercise (P.try eol <|> Comb.eof)
     pure $ Routine date exercises
     <?> "routine"

exercise :: P.Parser Exercise
exercise =
  do label <- Comb.many1 (C.noneOf ":0123456789\n")
     C.char ':' <?> "\":\" separating exercise label from sets"
     C.spaces
     -- TODO: why do comments stop the parsing ????????
     sets <- Comb.manyTill set (P.try eol <|> P.try Comb.eof <|> P.try comment)
     pure $ Exercise label sets Nothing
     <?> "exercise"

comment :: P.Parser ()
-- TODO: do not ignore the comment
comment =
  do C.spaces
     C.string "//"
     C.spaces
     tilEndOfLine
     pure ()
     <?> "comment"

set :: P.Parser Set
set = C.spaces *> (P.try fullSet <|> P.try withHelpSet <|> partialSet) <?> "set"

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

eol :: P.Parser ()
eol = C.oneOf "\n\r" >> pure () <?> "end of line"

tilEndOfLine :: P.Parser String
tilEndOfLine = Comb.many1 (C.noneOf "\r\n")
