{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Relation
  ( NamedRelation, RelationConcept
  , makeNR
  , makeRC
  ) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.ExprRelat

import Language.Drasil.NounPhrase (NP)

-- | A NamedRelation is just the combination of a 'NamedIdea' with a 'Relation'
data NamedRelation where 
  NR :: NamedIdea c => c -> Relation -> NamedRelation

-- | Helper for retrieving the NamedIdea from a NamedRelation and wrapping it as
-- an NWrapper
--namewrap :: NamedRelation -> NWrapper
--namewrap (NR c _) = nw c

instance Chunk NamedRelation where
  id = cp id

instance NamedIdea NamedRelation where
  term = cp term
  getA (NR c _) = getA c
  
instance ExprRelat NamedRelation where
  relat f (NR n r) = fmap (\x -> NR n x) (f r)
  
data RelationConcept where 
  RC :: Concept c => c -> Relation -> RelationConcept

instance Chunk RelationConcept where
  id = rcl id

instance NamedIdea RelationConcept where
  term = rcl term
  getA (RC c _) = getA c
  
instance Concept RelationConcept where
  defn = rcl defn
  cdom = rcl cdom

instance ExprRelat RelationConcept where
  relat f (RC c r) = fmap (\x -> RC c x) (f r)

-- don't export this
rcl:: (forall c. (Concept c) => Simple Lens c a) -> Simple Lens RelationConcept a
rcl l f (RC a b) = fmap (\x -> RC (set l x a) b) (f (a ^. l))

-- | Create a RelationConcept from a given id, term, defn, and relation.
makeRC :: String -> NP -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn rel = RC (dccWDS rID rTerm rDefn) rel

-- don't export this
cp :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens NamedRelation a
cp l f (NR a b) = fmap (\x -> NR (set l x a) b) (f (a ^. l))

-- | Create a NamedRelation from a given id, term, and relation.
makeNR :: String -> NP -> Relation -> NamedRelation
makeNR rID rTerm rel = NR (nc rID rTerm) rel
