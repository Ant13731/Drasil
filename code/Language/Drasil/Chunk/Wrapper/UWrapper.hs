{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper.UWrapper
  ( uw
  , UWrapper
  , ucw
  , UCWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import qualified Language.Drasil.Chunk.Quantity as Q
import Language.Drasil.Chunk.Unitary
import Prelude hiding (id)

-- | Unitary Wrapper
data UWrapper where
  UW :: (Unitary c) => c -> UWrapper
  
ulens :: (forall c. (Unitary c) => 
  Simple Lens c a) -> Simple Lens UWrapper a
ulens l f (UW a) = fmap (\x -> UW (set l x a)) (f (a ^. l))

instance Eq UWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Ord UWrapper where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((Q.getSymb Equational a) ^. symbol) ((Q.getSymb Equational b) ^. symbol)

instance Chunk UWrapper where
  id = ulens id
instance NamedIdea UWrapper where
  term = ulens term
  getA (UW a) = getA a
instance Q.Quantity UWrapper where
  typ = ulens Q.typ
  getSymb s  (UW a) = Q.getSymb s a
  getUnit    (UW a) = Q.getUnit a
  getStagedS (UW a) = Q.getStagedS a
instance Unitary UWrapper where
  unit (UW a) = unit a
  
-- | Constructor for Unital Wrappers. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
uw :: (Unitary c) => c -> UWrapper
uw = UW

-- | Unitary __and__ Concept Wrapper
data UCWrapper where
  UCW :: (Unitary c, Concept c) => c -> UCWrapper
  
uclens :: (forall c. (Unitary c, Concept c) => 
  Simple Lens c a) -> Simple Lens UCWrapper a
uclens l f (UCW a) = fmap (\x -> UCW (set l x a)) (f (a ^. l))

instance Eq UCWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Ord UCWrapper where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((Q.getSymb Equational a) ^. symbol) ((Q.getSymb Equational b) ^. symbol)

instance Chunk UCWrapper where
  id = uclens id
instance NamedIdea UCWrapper where
  term = uclens term
  getA (UCW a) = getA a
instance Q.Quantity UCWrapper where
  typ = uclens Q.typ
  getSymb s  (UCW a) = Q.getSymb s a
  getUnit    (UCW a) = Q.getUnit a
  getStagedS (UCW a) = Q.getStagedS a
instance Unitary UCWrapper where
  unit (UCW a) = unit a
instance Concept UCWrapper where
  defn = uclens defn
  cdom = uclens cdom
  
-- | Constructor for Unital Wrappers. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
ucw :: (Unitary c, Concept c) => c -> UCWrapper
ucw = UCW

