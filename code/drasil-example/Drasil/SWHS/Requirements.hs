module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil
import Utils.Drasil

import Drasil.DocLang (inReq)
import Drasil.DocLang.SRS (datCon, propCorSol) 

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (assumption, code, condition,
  funcReqDom, input_, likelyChg, mg, mis, module_, nonFuncReqDom, output_,
  physicalConstraint, property, requirement, simulation, srs, traceyMatrix,
  unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)
import Data.Drasil.Concepts.Thermodynamics as CT (lawConsEnergy, melting)

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (energy, time)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, thModel)

import Drasil.SWHS.DataDefs (waterMass, waterVolume, tankVolume, 
  balanceDecayRate, balanceDecayTime, balanceSolidPCM, balanceLiquidPCM)
import Drasil.SWHS.Concepts (phsChgMtrl, tank)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM, 
  iMods)
import Drasil.SWHS.Unitals (consTol, pcmE, tFinalMelt, tInitMelt, tempPCM, 
  tempW, watE)

------------------------------
-- Data Constraint: Table 1 --
------------------------------

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

inReqDesc :: Sentence
inReqDesc = foldlList Comma List [S "the" +:+ phrase tank +:+ plural parameter,
  plural materialProprty, S "initial" +:+ plural condition]

funcReqs :: [ConceptInstance]
funcReqs = [findMass, checkWithPhysConsts, outputInputDerivVals,
  calcTempWtrOverTime, calcTempPCMOverTime, calcChgHeatEnergyWtrOverTime,
  calcChgHeatEnergyPCMOverTime, verifyEnergyOutput, calcPCMMeltBegin, calcPCMMeltEnd]

findMass, checkWithPhysConsts, outputInputDerivVals, calcTempWtrOverTime,
  calcTempPCMOverTime, calcChgHeatEnergyWtrOverTime, calcChgHeatEnergyPCMOverTime,
  verifyEnergyOutput, calcPCMMeltBegin, calcPCMMeltEnd :: ConceptInstance

--
findMass = findMassConstruct (inReq EmptyS) (plural mass) iMods 
  [waterMass, waterVolume, tankVolume]

findMassConstruct :: (Referable r, HasShortName r, Referable s, HasShortName s,
  Referable t, HasShortName t) => r -> Sentence -> [s] -> [t] -> ConceptInstance
findMassConstruct fr m ims ddefs = cic "findMass" (foldlSent [
  S "Use the", plural input_ `sIn` makeRef2S fr, S "to find the", 
  m, S "needed for", foldlList Comma List (map makeRef2S ims) `sC`
  S "using", foldlList Comma List (map makeRef2S ddefs)])
  "Find-Mass" funcReqDom
--
checkWithPhysConsts = cic "checkWithPhysConsts" (foldlSent [
  S "Verify that the", plural input_, S "satisfy the required",
  plural physicalConstraint, S "shown in", makeRef2S (datCon ([]::[Contents]) ([]::[Section]))])
  "Check-Input-with-Physical_Constraints" funcReqDom
--
outputInputDerivVals = oIDQConstruct oIDQVals

oIDQConstruct :: [Sentence] -> ConceptInstance
oIDQConstruct x = cic "outputInputDerivVals" (foldlSentCol [
  titleize output_, S "the", plural inValue `sAnd`
  S "derived", plural value `inThe` S "following list"] +:+.
  foldlList Comma List x) "Output-Input-Derived-Values" funcReqDom

oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [S "the", plural value, S "from", makeRef2S (inReq EmptyS)],
  [S "the", plural mass, S "from", makeRef2S findMass],
  [ch balanceDecayRate, sParen (S "from" +:+ makeRef2S balanceDecayRate)],
  [ch balanceDecayTime, sParen (S "from" +:+ makeRef2S balanceDecayTime)],
  [ch balanceSolidPCM,  sParen (S "from" +:+ makeRef2S balanceSolidPCM)],
  [ch balanceLiquidPCM, sParen (S "from" +:+ makeRef2S balanceLiquidPCM)]
  ]
  
--
calcTempWtrOverTime = cic "calcTempWtrOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase tempW,
  sParen (ch tempW :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S eBalanceOnWtr)])
  "Calculate-Temperature-Water-Over-Time" funcReqDom
--
calcTempPCMOverTime = cic "calcTempPCMOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase tempPCM,
  sParen (ch tempPCM :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)])
  "Calculate-Temperature-PCM-Over-Time" funcReqDom
--
calcChgHeatEnergyWtrOverTime = cic "calcChgHeatEnergyWtrOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase watE,
  sParen (ch watE :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S heatEInWtr)])
  "Calculate-Change-Heat_Energy-Water-Over-Time" funcReqDom
--
calcChgHeatEnergyPCMOverTime = cic "calcChgHeatEnergyPCMOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase pcmE,
  sParen (ch pcmE :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S heatEInPCM)])
  "Calculate-Change-Heat_Energy-PCM-Over-Time" funcReqDom
--
verifyEnergyOutput = cic "verifyEnergyOutput" (foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch watE :+: sParen (ch time) `sAnd` ch pcmE :+:
  sParen (ch time)), S "follow the", phrase CT.lawConsEnergy `sC`
  S "as outlined in", makeRef2S (propCorSol [] []) `sC`
  S "with relative error no greater than", ch consTol])
  "Verify-Energy-Output-Follow-Conservation-of-Energy" funcReqDom
--
calcPCMMeltBegin = cic "calcPCMMeltBegin" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "begins to melt",
  ch tInitMelt, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)])
  "Calculate-PCM-Melt-Begin-Time" funcReqDom
--
calcPCMMeltEnd = cic "calcPCMMeltEnd" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  ch tFinalMelt, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)])
  "Calculate-PCM-Melt-End-Time" funcReqDom

-- List structure same between all examples

--How to include pi?
--How to add exponents?

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------

nfRequirements :: [ConceptInstance]
nfRequirements = [correct, verifiable, understandable, reusable, maintainable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  plural output_ `ofThe'` phrase code, S "have the",
  plural property, S "described in", makeRef2S (propCorSol [] [])
  ]) "Correct" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  S "The", phrase code, S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "The", phrase code, S "is modularized with complete",
  phrase mg `sAnd` phrase mis]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  S "The", phrase code, S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix, S "in the", getAcc srs `sAnd` phrase mg]) "Maintainable" nonFuncReqDom

-- The second sentence of the above paragraph is repeated in all examples (not
-- exactly, but the general idea is). The first sentence is not always
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be
-- abstracted out.
