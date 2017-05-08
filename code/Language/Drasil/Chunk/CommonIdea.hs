module Language.Drasil.Chunk.CommonIdea
  ( CommonIdea(..) --, commonidea, CI
  , CINP, commonINP
  ) where

import Prelude hiding (id)

import Language.Drasil.Chunk (Chunk(id))
import Language.Drasil.Chunk.NamedIdea
import Control.Lens (Simple, Lens)
import Language.Drasil.Spec (Sentence(S))
import Language.Drasil.NounPhrase

-- | CommonIdea is a chunk that is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: Simple Lens c Sentence
{-  
data CI = CI String Sentence Sentence

instance Chunk CI where
  id f (CI a b c) = fmap (\x -> CI x b c) (f a)
instance NamedIdea CI where
  term f (CI a b c) = fmap (\x -> CI a x c) (f b)
  getA (CI _ _ c) = Just c
instance CommonIdea CI where
  abrv f (CI a b c) = fmap (\x -> CI a b x) (f c)

commonidea :: String -> String -> String -> CI
commonidea i nm ab = CI i (S nm) (S ab)
-}
--FIXME: Change CINP to CI and remove Sentence (term).

-- | The common idea (with nounPhrase) data type. It must have a 
-- 'NounPhrase' for its 'term'.
data CINP = CINP String Sentence Sentence NP 
-- ^ The first Sentence here is now deprecated

instance Chunk CINP where
  id f (CINP a b c d) = fmap (\x -> CINP x b c d) (f a)
instance NamedIdea CINP where
  term f (CINP a b c d) = fmap (\x -> CINP a b c x) (f d)
  getA (CINP _ _ c _) = Just c
instance CommonIdea CINP where
  abrv f (CINP a b c d) = fmap (\x -> CINP a b x d) (f c)
instance NounPhrase CINP where
  phrase       (CINP _ _ _ d) = phrase d
  plural       (CINP _ _ _ d) = plural d
  sentenceCase (CINP _ _ _ d) = sentenceCase d
  titleCase    (CINP _ _ _ d) = titleCase d
  
-- | The commonINP smart constructor requires a chunk id, 
-- term (of type 'NP'), and abbreviation
commonINP :: String -> NP -> String -> CINP
commonINP i t a = CINP i (phrase t) (S a) t
