module Morphology where

{- This file implements an alternative morphology proposed by Ntsékees
 - on roljbogu'e.
 -
 - The idea is to separate brivla into the following classes:
 - * cluster-initial (e.g. drazu'e)
 - * two-syllable (e.g. gismu)
 - * multi-vowel-short-end (e.g. zu'edji)
 - * schwa-denoted (e.g. jesyspa)
 -
 - This allows for a simpler rule for determining how to split a sequence of
 - words with missing spaces (e.g. lonudrazu'e) into words.
 -
 - An important cost to note is that sequences of letterals under this system
 - must be followed by a glottal stop.  This makes it possible to tell whether
 - lony starts a lujvo or is lo + ny without looking at the lujvo as a whole.
 -}

import Data.List (intercalate)
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP, (<++), munch1)
import ReadPUtils

type CharPair = (Char, Char)

baseConsonants, baseVowels :: String
baseConsonants = "pbtdkgfvcjszxlmnr"
baseVowels = "aeiou"

-- We split consonants into strongly and weakly voiced.  Weakly voiced
-- consonants may be in a consonant pair with an unvoiced consonant,
-- while strongly voiced consonants may not be.
isStronglyVoiced :: Char -> Bool
isStronglyVoiced x = x `elem` "bdgvjz"

isWeaklyVoiced :: Char -> Bool
isWeaklyVoiced x = x `elem` "lmnr"

isUnvoiced :: Char -> Bool
isUnvoiced x = x `elem` "ptkfcsx"

devoice :: Char -> Char
devoice 'b' = 'p'
devoice 'd' = 't'
devoice 'g' = 'k'
devoice 'v' = 'f'
devoice 'j' = 'c'
devoice 'z' = 's'
devoice x = x

isLegalConsonantPair :: Char -> Char -> Bool
isLegalConsonantPair a b = not $ or bans
    where ab = [a, b]
          bans = [ a == b
                  , isStronglyVoiced a && isUnvoiced b
                  , isUnvoiced a && isStronglyVoiced b
                  , 'x' `elem` ab && any (`elem` ab) "ck" ]

allPairs :: String -> String -> [CharPair]
allPairs as bs = [(a, b) | a <- as, b <- bs]

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

initialConsonantPairs :: [CharPair]
initialConsonantPairs = concat [regVoicedPairs, regUnvoicedPairs, extraUnvoicedPairs, miscPairs]
    where regVoicedPairs = allPairs "bvg" "lr" ++ allPairs "d" "jrz" ++ allPairs "jz" "bvdgm"
          regUnvoicedPairs = mapPair devoice <$> regVoicedPairs
          extraUnvoicedPairs = allPairs "cs" "nlr"
          miscPairs = allPairs "mx" "lr"

isLegalInitialConsonantPair :: Char -> Char -> Bool
isLegalInitialConsonantPair a b = (a, b) `elem` initialConsonantPairs

isLegalConsonantTriple :: Char -> Char -> Char -> Bool
isLegalConsonantTriple a b c = and rules
    where rules = [ isLegalConsonantPair a b
                  , isLegalInitialConsonantPair b c
                  , not $ a == 'n' && devoice b == 't' && devoice c `elem` "cs"]


data CVRun = CVRun { consonants :: String, vowels :: [String] }
           deriving (Eq, Ord, Read, Show)

isConsonant, isVowel :: Char -> Bool
isConsonant x = x `elem` '.' : baseConsonants
isVowel x = x `elem` 'y' : baseVowels

pCVRun :: ReadP CVRun
pCVRun = CVRun <$> munch1 isConsonant <*> munchSepBy1 (munch1 isVowel) (P.char '\'')

getCVRuns :: String -> Maybe [CVRun]
getCVRuns s = case P.readP_to_S (genMunch pCVRun <* P.eof) s of
                ((r, _):_) -> Just r
                [] -> Nothing

showCVRun (CVRun cs vss) = cs ++ intercalate "'" vss

-- Checks whether this is a letteral of the form "Xy".  Note that we do not count ".y" as such,
-- meaning that words may start with a schwa.
isSchwaLetteral :: CVRun -> Bool
isSchwaLetteral (CVRun cs ["y"]) = cs /= "."
isSchwaLetteral _ = False

isMultiConsonant :: CVRun -> Bool
isMultiConsonant (CVRun cs _) = length cs > 1

splitIntoWords :: [CVRun] -> [[CVRun]]
splitIntoWords cs | all isSchwaLetteral cs = map return cs
splitIntoWords [] = []
splitIntoWords [cv] = [[cv]]
splitIntoWords a@(cv:dv:cvs) | isSchwaLetteral cv = error "Schwa letterals are not allowed at the beginning of a non-letteral word."
                             | isMultiConsonant cv = [a]
                             | isSchwaLetteral dv = [a]
                             | null cvs && isMultiConsonant dv = [a]
                             | otherwise = [cv] : splitIntoWords (dv:cvs)

stringToWords :: String -> Maybe [String]
stringToWords s = map showWord . splitIntoWords <$> getCVRuns s
    where showWord :: [CVRun] -> String
          showWord = concatMap showCVRun 
