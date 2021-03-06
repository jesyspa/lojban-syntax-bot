module Lexer where

import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP, (<++))
import Data.Char (isSpace)
import ReadPUtils

data CVRun = CVRun { consonants :: String, vowels :: String, user_cv_rep :: String } deriving (Eq, Ord, Read, Show)
data Valsi = Valsi { internal_rep :: String, user_rep :: String } deriving (Eq, Ord, Read, Show)

data SchwaStatus = SchwaVowel | SchwaConsonant deriving (Eq, Ord, Read, Show)
data StopStatus = StopAllowed | StopDisallowed deriving (Eq, Ord, Read, Show)

isVowel, isConsonant :: SchwaStatus -> Char -> Bool
isVowel s x = x `elem` "aeiou" || (x == 'y' && s == SchwaVowel)
isConsonant s x = x `elem` "bcdfgjklmnprstvxz" || (x == 'y' && s == SchwaConsonant)

-- Whether the CVRun can be one that indicates this thing starts a brivla.
isBrivlaInitial :: CVRun -> Bool
isBrivlaInitial cvrun = length (consonants cvrun) > 1

concatCVRuns :: [CVRun] -> Valsi
concatCVRuns = foldr go (Valsi [] [])
    where go (CVRun cs vs cvur) (Valsi ir vur) = Valsi (cs ++ vs ++ ir) (cvur ++ vur)

pConsonantRep :: StopStatus -> SchwaStatus -> ReadP String
pConsonantRep StopAllowed s = (++) <$> strictOption "" (P.string ".") <*> P.munch (isConsonant s)
pConsonantRep StopDisallowed s = P.munch (isConsonant s)

-- Parses a squence of vowels separated by '
pVowelRep :: SchwaStatus -> ReadP String
pVowelRep s = fst <$> P.gather (munchSepBy1 (P.munch1 $ isVowel s) (P.char '\''))

pCVRun :: StopStatus -> SchwaStatus -> ReadP CVRun
pCVRun stop schwa = go <$> pConsonantRep stop schwa <*> pVowelRep schwa
    where go cs vs = CVRun { consonants = csNorm, vowels = vs, user_cv_rep = cs ++ vs }
            where cs' = filter (/= '.') cs 
                  csNorm | null cs' = "."
                         | otherwise = cs'

-- None of these three accept the empty string.
-- That said, we do do a bit of a hack here.  When a parser has found any word, it stops
-- there and returns it.  We trust pValsi to be invoked from the same place.  This is kinda
-- weird and hacky, but hey.
--
-- We may be looking at zero or more cmavo, optionally followed by a brivla, or we may
-- just be looking at a cmevla.
-- There are a lot of tests for checking what kind of word things are, which we
-- currently ignore.  Our approach is simple:
-- * We start by assuming what we're seeing is a brivla.
-- * If we see more than one consonant in the first or second syllable (schwa counts as
--   a consonant for the second), that means we're right!  This whole thing is a brivla.
--   - This necessarily breaks things like {lobroda}!  We need a separate check there.
-- * If we can't identify it as a brivla, try viewing it as a cmavo instead.
-- * If it doesn't work even as a cmavo, just eat the whole word (up to whitespace) as a cmevla.
--
-- TODO: Implement the slinku'i and tosmabru tests (ki'e doi la ziren).
pValsi, pBrivla, pCmavo, pCmevla :: ReadP Valsi
pValsi = pBrivla <++ pCmavo <++ pCmevla
pCmevla = go <$> P.munch1 (not . isSpace)
    where go s = Valsi { internal_rep = s, user_rep = s }
pBrivla = go <$> pLead <*> genMunch (pCVRun StopDisallowed SchwaVowel)
    where go a b = concatCVRuns (a ++ b)
          pLead = do
              x <- pCVRun StopAllowed SchwaVowel
              y <- pCVRun StopDisallowed SchwaConsonant
              if isBrivlaInitial x || isBrivlaInitial y
                then return [x, y]
                else P.pfail
pCmavo = concatCVRuns . return <$> pCVRun StopAllowed SchwaVowel

pText :: ReadP [Valsi]
pText = strictOption [] (munchSepBy1 pValsi P.skipSpaces) <* P.eof

parse = P.readP_to_S pText
parseAndPrettyPrint s =
    let (vs, _) = head $ parse s
    in unwords $ map internal_rep vs
