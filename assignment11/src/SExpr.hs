module SExpr where
import AParser
import Control.Applicative
import Data.Char

-- from last lecture

(*>*) :: Applicative f => f a -> f b -> f b
(*>*) a b = (id <$ a) <*> b

mapA':: Applicative f => (a -> f b) -> [a] -> f [b]
mapA' f = sequenceA' . fmap f

sequenceA':: Applicative f => [f a] -> f [a]
sequenceA'= traverse id

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA i a = sequenceA' $ take i $ repeat a


------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

oneOrMore :: Parser a -> Parser [a]
oneOrMore pa = liftA2 (:) pa (zeroOrMore pa)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore a = oneOrMore a <|> pure []

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (++) (oneOrMore (satisfy isAlpha)) (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr = atomParser <|> combParser
  where
    atom =  A <$> (N <$> posInt <|> I <$> ident)
    atomParser = spaces *> atom <* spaces
    combParser = Comb <$> oneOrMore
      (zeroOrMore (char '(')
       *>
       atomParser
       <*
       zeroOrMore (char ')'))
