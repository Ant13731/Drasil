-- | Standard code to make a table of symbols.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableOfAbbAndAcronyms, tableAbbAccUID) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (abbreviation, fullForm)

import Data.List (sortBy)
import Data.Function (on)
-- | Creates a standard table of abbreviations and acronyms section from a
-- given list of abbreviated chunks.
tableOfAbbAndAcronyms :: (Idea s) => [s] -> Section
tableOfAbbAndAcronyms ls = Section (S (snd tableAbbAccUID))
  [Con (LlC $ table ls)] (uncurry makeSecRef tableAbbAccUID)

-- | Helper function that gets the acronym out of an 'Idea'.
select :: (Idea s) => [s] -> [(String, s)]
select [] = []
select (x:xs) = case getA x of
  Nothing -> select xs
  Just y  -> (y, x) : select xs

-- | The actual table creation function.
table :: (Idea s) => [s] -> LabelledContent
table ls = let chunks = sortBy (compare `on` fst) $ select ls in
  llcc (makeTabRef (fst tableAbbAccUID)) $ Table
  (map titleize [abbreviation, fullForm]) (mkTable
  [\(a,_) -> S a,
   \(_,b) -> titleize b]
  chunks)
  (S (snd tableAbbAccUID)) True

-- | Table of abbreviations and acronyms UID and shortname.
tableAbbAccUID :: (UID, String)
tableAbbAccUID = ("TAbbAcc", "Abbreviations and Acronyms")
