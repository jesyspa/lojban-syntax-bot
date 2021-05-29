module ReadPUtils where

import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP, (<++))

-- Some redefinitions that avoid backtracking.
strictOption :: a -> ReadP a -> ReadP a
strictOption a p = p <++ return a

pCons :: ReadP a -> ReadP [a] -> ReadP [a]
pCons a as = (:) <$> a <*> as

-- Note: don't use on a parser that accepts the empty string or you'll have a bad time.
genMunch :: ReadP a -> ReadP [a]
genMunch a = strictOption [] $ pCons a $ genMunch a

guardParse :: (a -> Bool) -> ReadP a -> ReadP a
guardParse f p = p >>= \a -> if f a then return a else P.pfail

-- At least one of a and sep must reject the empty string.
munchSepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
munchSepBy1 a sep = pCons a $ genMunch $ sep *> a

