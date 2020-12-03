module Drasil.Pendulum.Requirements where

import Language.Drasil
import Drasil.DocLang.SRS (datCon, propCorSol)
import Utils.Drasil

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (datumConstraint, funcReqDom,
        output_, value,  nonFuncReqDom, code, property, environment)
--  likelyChg, mg, mis, module_, nonFuncReqDom,
--   requirement, srs, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)
import Drasil.Pendulum.IMods (calAngularAccelerationIM)
import Drasil.Pendulum.Unitals (lenRod, pendDisplacementAngle)
import Data.Drasil.Quantities.Physics (angularAccel)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [verifyInptVals, calcAngPos, outputValues]

verifyInptVals, calcAngPos, outputValues :: ConceptInstance

verifyInptVals = cic "verifyInptVals" verifyInptValsDesc "Verify-Input-Values" funcReqDom
calcAngPos  = cic "calcAngPos"   calcAngPosDesc   "Calculate-Angular-Position-Of-Mass" funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

verifyInptValsDesc, calcAngPosDesc, outputValuesDesc :: Sentence

verifyInptValsDesc = foldlSent [S "Check the entered", plural inValue,
  S "to ensure that they do not exceed the", plural datumConstraint,
  S "mentioned in" +:+. makeRef2S (datCon ([]::[Contents]) ([]::[Section])), 
  S "If any of the", plural inValue, S "are out of bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `andThe` plural calculation, S "stop"]

calcAngPosDesc = foldlSent [S "Calculate the following" +: plural value,
  foldlList Comma List [
    ch angularAccel +:+ sParen (S "from" +:+ makeRef2S calAngularAccelerationIM),  
    ch pendDisplacementAngle   +:+ sParen (S "from" +:+ makeRef2S calAngularAccelerationIM)
  ]]
outputValuesDesc = foldlSent [atStart output_, ch lenRod,
  sParen (S "from" +:+ makeRef2S calAngularAccelerationIM) `sAnd` ch lenRod,
  sParen (S "from" +:+ makeRef2S calAngularAccelerationIM)]


{--Nonfunctional Requirements--}

nonFuncReqs :: [ConceptInstance]
nonFuncReqs = [correct, portable]


correct :: ConceptInstance
correct = cic "correct" (foldlSent [
 plural output_ `ofThe'` phrase code, S "have the",
 plural property, S "described in", makeRef2S (propCorSol [] [])
 ]) "Correct" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The", phrase code, S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom
 
