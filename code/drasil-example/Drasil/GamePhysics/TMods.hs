{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.TMods (tMods, newtonSL, newtonSLR, newtonTL, newtonLUG) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tmNoRefs, ModelKinds(OthModel, EquationalModel))
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Drasil.GamePhysics.Assumptions (assumpOD)
import Drasil.GamePhysics.Unitals (dispNorm, dVect, force_1, force_2,
  mass_1, mass_2, sqrDist, distMass)

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Physics (rigidBody, twoD)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (angularAccel,
  force, gravitationalConst, momentOfInertia, torque)
import Data.Drasil.Theories.Physics (newtonSL)

----- Theoretical Models -----

tMods :: [TheoryModel]
tMods = [newtonSL, newtonTL, newtonLUG, newtonSLR]

-- T1 : Newton's second law of motion --

-- T2 : Newton's third law of motion --

newtonTL :: TheoryModel
newtonTL = tmNoRefs (EquationalModel newtonTLQD) [qw force_1, qw force_2]
  ([] :: [ConceptChunk]) [] [relat newtonTLQD] [] "NewtonThirdLawMot" [newtonTLNote]

newtonTLQD :: QDefinition
newtonTLQD = mkQuantDef' force_1 (nounPhraseSP "Newton's third law of motion") newtonTLExpr

newtonTLExpr :: Expr
newtonTLExpr = negate (sy force_2)

newtonTLNote :: Sentence
newtonTLNote = foldlSent [(S "Every action has an equal and opposite reaction" !.),
  S "In other words, the", phrase force, ch force_1, S "exerted on the second",
  phrase rigidBody, S "by the first is equal in magnitude and in the opposite direction" `S.toThe`
  phrase force, ch force_2, S "exerted on the first", phrase rigidBody, S "by the second"]

-- T3 : Newton's law of universal gravitation --

newtonLUG :: TheoryModel
newtonLUG = tmNoRefs (OthModel newtonLUGRC)
  [qw force, qw gravitationalConst, qw mass_1, qw mass_2,
  qw dispNorm, qw dVect, qw distMass] ([] :: [ConceptChunk])
  [] [newtonLUGRel] [] "UniversalGravLaw" newtonLUGNotes

newtonLUGRC :: RelationConcept
newtonLUGRC = makeRC "newtonLUGRC" 
  (nounPhraseSP "Newton's law of universal gravitation") EmptyS newtonLUGRel

newtonLUGRel :: Relation
newtonLUGRel = sy force $=
  sy gravitationalConst * (sy mass_1 * sy mass_2 /
  (sy dispNorm $^ 2)) * sy dVect $=
  sy gravitationalConst * (sy mass_1 * sy mass_2 /
  (sy dispNorm $^ 2)) * (sy distMass / sy dispNorm)

-- Can't include fractions within a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184)). Changed to a
-- verbal description instead.

-- Can't properly include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 * 10^{-11} m/kgs^2" (line 187)).

newtonLUGNotes :: [Sentence]
newtonLUGNotes = map foldlSent [
  [S "Two", plural rigidBody `S.inThe` S "universe attract each other with a",
   getTandS force, S "that is directly proportional to the product of their",
   plural mass `sC` ch mass_1 `S.and_` ch mass_2 `sC` EmptyS `S.and_`
   S "inversely proportional" `S.toThe` getTandS sqrDist, S "between them"]]

-- T4 : Newton's second law for rotational motion --

newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (OthModel newtonSLRRC)
  [qw torque, qw momentOfInertia, qw angularAccel] 
  ([] :: [ConceptChunk]) [] [newtonSLRRel] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRRC :: RelationConcept
newtonSLRRC = makeRC "newtonSLRRC" 
  (nounPhraseSP "Newton's second law for rotational motion") EmptyS newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = sy torque $= sy momentOfInertia * sy angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = map foldlSent [
  [S "The net", getTandS torque, S "on a", phrase rigidBody `S.is`
   S "proportional to its", getTandS angularAccel `sC` S "where",
   ch momentOfInertia, S "denotes", phrase momentOfInertia `S.the_ofThe`
   phrase rigidBody, S "as the", phrase constant `S.of_` S "proportionality"],
  [S "We also assume that all", plural rigidBody, S "involved" `S.are`
   phrase twoD, fromSource assumpOD]]
