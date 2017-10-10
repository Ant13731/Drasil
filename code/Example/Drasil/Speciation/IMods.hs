module Drasil.Speciation.IMods where

import Language.Drasil

import Data.Drasil.SentenceStructures (acroA, foldlSent, isThe, sAnd, sOr)
import Data.Drasil.Utils (getS)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.Documentation (coordinate)

import Prelude hiding (exp)
import Control.Lens ((^.))

{-
import Drasil.GlassBR.Unitals (tNT, sdWithEqn, demand, standOffDist, 
  char_weight, eqTNTWeight, demandq, sdVectorSent, wtntWithEqn, loadSF,
  lRe, risk_fun, prob_br)
import Drasil.GlassBR.DataDefs (nonFL, risk, glaTyFac)
import Drasil.GlassBR.Concepts (lResistance, glassTypeFac, lShareFac)

import Drasil.GlassBR.ModuleDefs

{--}

iModels :: [RelationConcept]
iModels = [probOfBr, calOfCap, calOfDe]

{--}

probOfBr :: RelationConcept
probOfBr = makeRC "probOfBr" (nounPhraseSP "Probability of Glass Breakage")
  pbdescr $ (C prob_br) := 1 - (exp (Neg (C risk)))

pbdescr :: Sentence
pbdescr =
  foldlSent [(getS prob_br) `isThe` (S "calculated" +:+. (phrase prob_br)),
  (getS risk_fun) `isThe` (phrase risk)]

{--}

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" (nounPhraseSP "Calculation of Capacity(LR)") 
  capdescr $ (C lRe) := ((C nonFL) * (C glaTyFac) * (C loadSF))

capdescr :: Sentence
capdescr =
  foldlSent [(getS lRe) `isThe` (phrase lResistance) `sC`
  S "which" +:+. S "is also called capacity" +:+. ((getS nonFL) `isThe`
  (phrase nonFL)) +:+. ((getS glaTyFac) `isThe` (phrase glassTypeFac))
  +:+. ((getS loadSF) `isThe` (phrase lShareFac)), S "Follows",
  (acroA 2) `sAnd` (acroA 1), sParen (Quote 
  (S "In the development of this procedure, it was assumed that" +:+
  S "all four edges of the glass are simply supported and free to slip" +:+
  S "in the plane of the glass. This boundary condition has been shown" +:+
  S "to be typical of many glass installations")) +:+ S "from [4 (pg. 53)]"
  {-astm_LR2009-}]

{--}

calOfDe :: RelationConcept
calOfDe = makeRC "calOfDe" (nounPhraseSP "Calculation of Demand(q)") 
  --dedescr $ (C demand) := FCall (C demand) [C eqTNTWeight, C standOffDist] 
  dedescr $ (C demand) := FCall (asExpr interpY) [V "TSD.txt", C standOffDist, C eqTNTWeight] 
  
dedescr :: Sentence
dedescr = 
  foldlSent [(getS demand `sOr` phrase demandq) `sC`
  S "is the", (demandq ^. defn), 
  S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (getS standOffDist) `sAnd`
  (getS eqTNTWeight), S "as" +:+. plural parameter, 
  (getS eqTNTWeight), S "is defined as" +:+.
  E (equat wtntWithEqn) +:+. ((getS char_weight) `isThe`
  (phrase char_weight)) +:+. ((getS tNT) `isThe`
  (phrase tNT)), (getS standOffDist) `isThe`
  (phrase standOffDist), S "where", E (equat sdWithEqn), S "where",
  sParen (sdVectorSent), S "are", plural coordinate]
-}