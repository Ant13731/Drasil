{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Unit (
    UDefn(..)                   -- languages
  , Unit(..), UnitEq(..)        -- classes
  , FundUnit(..), DerUChunk(..) -- data-structures
  , UnitDefn(..)                -- wrapper for 'Unit' class
  , from_udefn, makeDerU, unitCon
  , (^:), (/:), (*:), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  ) where

import Prelude hiding (id)

import Control.Lens (Simple, Lens, set, (^.))

import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..))
import Language.Drasil.Chunk.Concept (Concept(..), ConceptChunk, dcc)
import Language.Drasil.NounPhrase
import Language.Drasil.Spec (USymb(..))
import Language.Drasil.Symbol

import Language.Drasil.NounPhrase (cn')

-- | Language of units (how to build them up)
-- UName for the base cases, otherwise build up.
-- Probably a 7-vector would be better (less error-prone!)

data UDefn = USynonym USymb      -- ^ to define straight synonyms
           | UScale Double USymb -- ^ scale, i.e. *
           | UShift Double USymb -- ^ shift, i.e. +

-- | Units are concepts
class Concept u => Unit u where
   usymb :: Simple Lens u USymb

class UnitEq u where
   uniteq :: Simple Lens u UDefn


-- | Can generate a default symbol
from_udefn :: UDefn -> USymb
from_udefn (USynonym x) = x
from_udefn (UScale _ s) = s
from_udefn (UShift _ s) = s

-- | Create a derived unit chunk from a concept and a unit equation
makeDerU :: ConceptChunk -> UDefn -> DerUChunk
makeDerU concept eqn = DUC (UD concept (from_udefn eqn)) eqn

-- | Create a derived unit chunk from an id, term (as 'String'), definition,
-- symbol, and unit equation
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> DerUChunk
-- | Uses self-plural term
derUC  a b c s u = DUC (UD (dcc a (cn b) c) (UName $ s)) u
-- | Uses term that pluralizes by adding *s* to the end
derUC' a b c s u = DUC (UD (dcc a (cn' b) c) (UName $ s)) u

-- | Create a derived unit chunk from an id, term (as noun phrase), definition, 
-- symbol, and unit equation
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> DerUChunk
derUC'' a b c s u = DUC (UD (dcc a b c) (UName $ s)) u

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same string
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = dcc s (cn' s) s
---------------------------------------------------------

-- | for defining fundamental units
data FundUnit = UD { _vc :: ConceptChunk, _u :: USymb }

-- don't export this
vc :: Simple Lens FundUnit ConceptChunk
vc f (UD a b) = fmap (\x -> UD x b) (f a)

instance Chunk FundUnit where
  id = vc . id

instance NamedIdea FundUnit where
  term   = vc . term
  getA c = getA (c ^. vc)

instance Concept FundUnit where
  defn = vc . defn
  cdom = vc . cdom
  
instance Unit FundUnit where
  usymb f (UD a b) = fmap (\x -> UD a x) (f b)

-- | for defining Derived units
data DerUChunk = DUC { _uc :: FundUnit, _eq :: UDefn }

-- don't export this either
duc :: Simple Lens DerUChunk FundUnit
duc f (DUC a b) = fmap (\x -> DUC x b) (f a)

instance Chunk     DerUChunk where id  = duc . id
instance NamedIdea DerUChunk where
  term = duc . term
  getA c = getA (c ^. duc)
instance Concept   DerUChunk where 
  defn = duc . defn
  cdom = duc . cdom
instance Unit      DerUChunk where usymb  = duc . usymb

instance UnitEq DerUChunk where
  uniteq f (DUC a b) = fmap (\x -> DUC a x) (f b)

----------------------------------------------------------

-- | For allowing lists to mix the two, thus forgetting
-- the definition part
data UnitDefn where
  UU :: Unit u => u -> UnitDefn

-- don't export
ulens :: (forall u. Unit u => Simple Lens u a) -> Simple Lens UnitDefn a
ulens l f (UU a) = fmap (\x -> UU (set l x a)) (f (a ^. l))

instance Chunk     UnitDefn where id   = ulens id
instance NamedIdea UnitDefn where
  term = ulens term
  getA (UU a) = getA a
instance Concept   UnitDefn where 
  defn = ulens defn
  cdom = ulens cdom
instance Unit      UnitDefn where usymb = ulens usymb

--- These conveniences go here, because we need the class
-- | Combinator for raising a unit to a power
(^:) :: Unit u => u -> Integer -> USymb
u ^: i = UPow (u ^. usymb) i

-- | Combinator for dividing one unit by another
(/:) :: (Unit u1, Unit u2) => u1 -> u2 -> USymb
u1 /: u2 = UDiv (u1 ^. usymb) (u2 ^. usymb)

-- | Combinator for multiplying two units together
(*:) :: (Unit u1, Unit u2) => u1 -> u2 -> USymb
u1 *: u2 = UProd [(u1 ^. usymb), (u2 ^. usymb)]

-- | Combinator for scaling one unit by some number
scale :: Unit s => Double -> s -> UDefn
scale a b = UScale a (b ^. usymb)

-- | Combinator for shifting one unit by some number
shift :: Unit s => Double -> s -> UDefn
shift a b = UShift a (b ^. usymb)

-- | Smart constructor for new derived units from existing units.
new_unit :: String -> USymb -> DerUChunk
new_unit s u = makeDerU (unitCon s) (USynonym u)
