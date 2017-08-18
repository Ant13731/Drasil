module Drasil.SSP.IMods where
--(sspIMods), currently weaves the derivations in body.hs

import Prelude hiding (tan, product, sin, cos)

import Language.Drasil
import Drasil.SSP.Unitals
import Drasil.SSP.Defs (slope, slice, slip,
  intrslce, ssa, morPrice, crtSlpSrf, factorOfSafety)
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Data.Drasil.Utils (getS)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (analysis,
  solution, definition, value, assumption, physicalProperty,
  problem, method_)
import Data.Drasil.SentenceStructures (andThe,
  acroA, acroGD, acroDD, sIs, sIn, getTDS,
  getTandS, ofThe, ofThe', sAnd, sOf,
  acroIM, acroT, eqN, foldlSP, foldlSent_)
import Control.Lens ((^.))
import Data.Drasil.Concepts.Math (equation, surface)
import Data.Drasil.Concepts.Physics (displacement, force)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Drasil.SSP.GenDefs (eqlExpr, momExpr)

-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [RelationConcept]
sspIMods = [fctSfty, nrmShrFor, intsliceFs, forDisEqlb, rfemFoS, crtSlpId]

--
fctSfty :: RelationConcept
fctSfty = makeRC "fctSfty" factorOfSafety fcSfty_desc fcSfty_rel

--FIXME: first shearRNoIntsl should have local index v, not i,
--       last occurence should have index n
--       similar case with shearFNoIntsl
fcSfty_rel :: Relation
fcSfty_rel = C fs := sumOp shearRNoIntsl / sumOp shearFNoIntsl
  where prodOp = product (Just (lU, Low $ C index, High $
          C numbSlices - Int 1))
          (Index (C mobShrC) (C varblU) / Index (C shrResC) (C varblU))
        sumOp sym = summation (Just (lV, Low $ Int 1, High $
          C numbSlices - Int 1))
          (Index (C sym) (C varblV) :* prodOp) + Index (C sym) (C numbSlices)

fcSfty_desc :: Sentence
fcSfty_desc = foldlSent [S "Equation for the", titleize fs `isThe` S "ratio",
  S "between resistive and mobile shear of the slip surface.",
  S "The sum of values from each slice is taken to find the total",
  S "resistive and mobile shear for the slip surface. The constants",
  getS shrResC, S "and", getS mobShrC, S "convert the resistive and",
  S "mobile shear without the inluence of",
  --FIXME: have these constants defined somewhere else
  S "interslice forces, to a calculation considering the interslice forces"]

--
nrmShrFor :: RelationConcept
nrmShrFor = makeRC "nrmShrFor" (nounPhraseSP "normal/shear force ratio")
  nrmShrF_desc nrmShrF_rel

nrmShrF_rel :: Relation
nrmShrF_rel = (inxi normFunc) := Case [case1,case2,case3]:=
  inxi shearFunc := Case [
  (indx1 baseWthX * indx1 scalFunc * indx1 intNormForce, C index := Int 1),
  (inxi baseWthX * (inxi scalFunc * inxi intNormForce +
    inx scalFunc (-1) * inx intNormForce (-1)),
    Int 2 :<= C index :<= (C numbSlices :- Int 1)),
  (indxn baseWthX * Index (C intNormForce) (C numbSlices - Int 1) *
    Index (C watrForce) (C numbSlices - Int 1), C index := Int 1)
  ]
  := --FIXME: move to seperate instance model
  C normToShear := sum1toN (inxi normFunc) / sum1toN (inxi shearFunc)
  where case1 = ((indx1 baseWthX)*((indx1 intNormForce)+(indx1 watrForce)) *
          tan (indx1 baseAngle), C index := Int 1)
        case2 = ((inxi baseWthX)*(Grouping (inxi intNormForce +
          inx intNormForce (-1))+ Grouping (inxi watrForce +
          inx watrForce (-1))) * tan (inxi baseAngle)+
          (C midpntHght) * (C earthqkLoadFctr * inxi slcWght -
          Int 2 * inxi surfHydroForce * sin (inxi surfAngle) -
          Int 2 * inxi surfLoad * cos (inxi impLoadAngle)),
          Int 2 :<= C index :<= ((C numbSlices) - (Int 1)))
        case3 = ((indxn baseWthX)*(Index (C intNormForce)
          (C numbSlices - Int 1) + Index (C watrForce)
          (C numbSlices - Int 1)) * tan (Index (C baseAngle)
          (C numbSlices - Int 1)), C index := (C numbSlices))

nrmShrF_desc :: Sentence
nrmShrF_desc = foldlSent [getS normToShear `isThe` S "magnitude ratio",
  S "between shear and normal forces at the interslice interfaces as the", 
  S "assumption of the Morgenstern Price method in", acroGD 5,
  S "The inclination function", getS scalFunc,
  S "determines the relative magnitude ratio between the",
  S "different interslices, while", getS normToShear, S "determines the" +:+.
  S "magnitude", getS normToShear, S "uses the sum of interslice normal",
  S "and shear forces taken from each interslice"]

--
intsliceFs :: RelationConcept
intsliceFs = makeRC "intsliceFs" (nounPhraseSP "interslice forces")
  sliceFs_desc sliceFs_rel

sliceFs_rel :: Relation
sliceFs_rel = inxi intNormForce := Case [
  (((C fs) * indx1 shearFNoIntsl - indx1 shearRNoIntsl) / indx1 shrResC,
    C index := (Int 1)),
  ((inx mobShrC (-1) * inx intNormForce (-1) +
    C fs * inxi shearFNoIntsl - inxi shearRNoIntsl) / inxi shrResC,
    (Int 2) :<= C index :<= ((C numbSlices) :- (Int 1))),
  ((Int 0), C index := (Int 0) :|| C index := C numbSlices)]  
  -- FIXME: Use index i as part of condition

sliceFs_desc :: Sentence
sliceFs_desc = foldlSent [S "The value of the interslice normal force",
  getS intNormForce, S "at interface", getS index +:+. S "The net force"
  `isThe` S "weight",
  S "of the slices adjacent to interface", getS index,
  S "exert horizontally on each other"]

--
forDisEqlb :: RelationConcept
forDisEqlb = makeRC "forDisEqlb"
  (nounPhraseSP "force displacement equilibrium") fDisEq_desc fDisEq_rel

fDisEq_rel :: Relation --FIXME: split into two IMOD
fDisEq_rel = Neg (inxi watrForceDif) - (C earthqkLoadFctr)*(inxi slcWght) -
  (inxi baseHydroForce)*(sin(inxi baseAngle)) +
  (inxi surfHydroForce)*sin(inxi surfAngle) + (inxi surfLoad) *
  sin(inxi impLoadAngle) := inx  dx_i (-1) * (Neg (inx surfLngth (-1)) *
  inx nrmStiffIntsl (-1)) + inxi dx_i * (Neg (inx surfLngth (-1)) *
  inx nrmStiffIntsl (-1) +
  inxi surfLngth * inxi nrmStiffIntsl + inxi baseLngth * inxi effStiffA) +
  inx  dx_i 1 * (Neg (inxi surfLngth) * inxi nrmStiffIntsl) +
  inxi dy_i * (Neg (inxi baseLngth) * inxi effStiffB) :=
  Neg (inxi slcWght) - (inxi baseHydroForce)*(cos(inxi baseAngle)) +
  (inxi surfHydroForce)*cos(inxi surfAngle) + (inxi surfLoad) *
  cos(inxi impLoadAngle) := inx  dy_i (-1) * (Neg (inx surfLngth (-1)) *
  inx shrStiffIntsl (-1)) + inxi dy_i * (Neg (inx surfLngth (-1)) *
  inx shrStiffIntsl (-1) + inxi surfLngth * inxi nrmStiffIntsl +
  inxi baseLngth * inxi effStiffA) + inx  dy_i 1 * (Neg (inxi surfLngth) *
  inxi shrStiffIntsl) + inxi dx_i * (Neg (inxi baseLngth) * inxi effStiffB)

fDisEq_desc :: Sentence
fDisEq_desc = foldlSent [
  S "There is one set of force displacement equilibrium equations",
  S "in the x and y directions for each element. System of equations",
  S "solved for displacements (", (getS dx_i), S "and", (getS dy_i), S ")",
  (getS watrForceDif), S "=",
  (getS watrForce) `isThe` S "net hydrostatic force across a slice.", 
  (getS earthqkLoadFctr) `isThe` S "earthquake load factor.",
  (getS slcWght) `isThe` S "weight of the slice.",
  (getS baseHydroForce)  `isThe` S "pore water pressure acting on the",
  S "slice base.",
  (getS surfHydroForce) `isThe` S "pore water pressure acting on the",
  S "slice surface.",
  (getS baseAngle) `isThe` S "angle of the base with the horizontal.",
  (getS surfAngle) `isThe` S "angle of the surface with the horizontal.",
  (getS dx_i) `isThe` S "x displacement of slice i.",
  (getS dy_i) `isThe` S "y displacement of slice i.",
  (getS surfLngth) `isThe` S "length of the interslice surface i.",
  (getS baseLngth) `isThe` S "length of the base surface i.",
  (getS shrStiffIntsl) `isThe` S "interslice shear stiffness at surface i.",
  S " Kst,i-1" `isThe` S "interslice normal stiffness at surface i.",
  S "KbA,i, and KbB,i", S "are the base stiffness values for slice i"]

--
rfemFoS :: RelationConcept
rfemFoS = makeRC "rfemFoS" (nounPhraseSP "RFEM factor of safety")
  rfemFoS_desc rfemFoS_rel

rfemFoS_rel :: Relation
rfemFoS_rel = (inxi fsloc) := fosFracLoc := fosFracSum

fosFracLoc :: Expr
fosFracLoc = (inxi cohesion - inxi nrmStiffBase * inxi nrmDispl *
  tan(inxi fricAngle)) / (inxi shrStiffBase * inxi shrDispl)

fosFracSum :: Expr
fosFracSum = sum1toN
  (inxi baseLngth * (inxi cohesion - inxi nrmStiffBase * inxi nrmDispl
    * tan(inxi fricAngle))) /
  sum1toN (inxi baseLngth * Grouping (inxi shrStiffBase * inxi shrDispl))

rfemFoS_desc :: Sentence
rfemFoS_desc = foldlSent [
  (getS fsloc) `isThe` S "factor of safety for slice i.",
  (getS fs) `isThe` S "factor of safety for the entire slip surface.",
  (getS cohesion) `isThe` S "cohesion of slice i's base.",
  (getS fricAngle) `isThe` (phrase fricAngle), S "of slice i's base.",
  (getS nrmDispl) `isThe` S "normal displacement of slice i.",
  (getS shrDispl) `isThe` S "shear displacement of slice i.",
  (getS shrStiffBase) `isThe` S "length of the base of slice i.",
  (getS nrmStiffBase) `isThe` S "base normal stiffness at surface i.",
  (getS numbSlices) `isThe` S "number of slices in the slip surface"]

--
crtSlpId :: RelationConcept
crtSlpId = makeRC "crtSlpId" (nounPhraseSP "critical slip identification")
  crtSlpId_desc crtSlpId_rel

crtSlpId_rel :: Relation
crtSlpId_rel = (Index (C fs) (V "min")) :=
  (FCall (C minFunction) [C critCoords, V "Input"])
  --FIXME: add subscript to fs

crtSlpId_desc :: Sentence
crtSlpId_desc = foldlSent [S "Given the necessary", phrase slope,
  S "inputs, a minimization", S "algorithm or function", getS minFunction,
  S "will identify the", phrase crtSlpSrf, S "of the", phrase slope `sC`
  S "with the critical", phrase slip, S "coordinates", getS critCoords, 
  S "and the minimum", phrase fs, E $ Index (C fs) (V "min"), S "that results"]

-----------
-- Intro --
-----------

instModIntro1, instModIntro2 :: Contents

instModIntro1 = foldlSP [S "The", titleize morPrice,
  phrase method_, S "is a vertical", phrase slice `sC` S "limit equilibrium",
  phrase ssa +:+. phrase method_, at_start analysis, S "is performed by",
  S "breaking the assumed failure", phrase surface,
  S "into a series of vertical", plural slice, S "of" +:+. phrase mass,
  S "Static equilibrium analysis using two", phrase force,
  S "equilibrium, and one moment", phrase equation, S "as in" +:+. acroT 2,
  S "The", phrase problem, S "is statically indeterminate with only these 3",
  plural equation, S "and one constitutive", phrase equation,
  sParen $ S "the Mohr Coulomb shear strength of" +:+
  acroT 3, S "so the", phrase assumption, S "of", acroGD 5,
  S "is used. Solving for", phrase force, S "equilibrium allows",
  plural definition, S "of all", plural force, S "in terms of the",
  plural physicalProperty, S "of", acroDD 1, S "to",
  acroDD 9 `sC` S "as done in", acroDD 10 `sC` acroDD 11]

instModIntro2 = foldlSP [
  plural value `ofThe'` (phrase intrslce +:+ phrase totNrmForce),
  getS intNormForce, S "the", getTandS normToShear `sC`
  S "and the", titleize fs, (sParen $ getS fs) `sC` S "are unknown.",
  at_start' equation, S "for the unknowns are written in terms of only the",
  plural value, S "in", acroDD 1, S "to", acroDD 9 `sC` S "the", plural value,
  S "of", getS shearRNoIntsl `sC` S "and", getS shearFNoIntsl, S "in",
  acroDD 10, S "and", acroDD 11 `sC` S "and each",
  S "other. The relationships between the unknowns are non linear" `sC`
  S "and therefore explicit", plural equation, S "cannot be derived and an",
  S "iterative", plural solution, S "method is required"]

-----------------
-- Derivations --
-----------------

-- FIXEME: move derivations with the appropriate instance model

fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation :: [Contents]

fctSftyDerivation = [foldlSP [S "Using", eqN 21, S "from", acroIM 3 `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", acroIM 1],
  
  EqnBlock fcSfty_rel,
  
  fUnknowns]

boundaryCon :: Sentence
boundaryCon = foldlSent_ [S "applying the boundary condition that",
  --FIXME: Index
  E (Index (C intNormForce) (Int 0)) `sAnd`
  E (indxn intNormForce), S "are equal to", E $ Int 0]

fUnknowns :: Contents
fUnknowns = foldlSP [S "The constants", getS mobShrC `sAnd` getS shrResC, 
  S "described in", eqN 20 `sAnd` eqN 19,
  S "are functions of the unknowns: the", getTandS normToShear,
  sParen (acroIM 2) `andThe` getTandS fs, sParen (acroIM 1)]

nrmShrDerivation = [

  foldlSP [S "Taking the last static", phrase equation,
  S "of", acroT 2, S "with the", S "moment equilibrium" `sOf` acroGD 6,
  S "about", (S "midpoint" `ofThe` S "base") `sAnd` S "the",
  phrase assumption, S "of", acroGD 5, S "results in", eqN 13],
  
  EqnBlock $ Int 0 :=
  momExpr (\ x y -> x :- (C normToShear * (inxi baseWthX / Int 2) * 
  (inxi intNormForce * inxi scalFunc + inxiM1 intNormForce *
  inxiM1 scalFunc)) :+ y),
  
  foldlSP [S "The", phrase equation, S "in terms of", getS normToShear,
  S "leads to", eqN 14],
  
  EqnBlock $
  C normToShear := momExpr (+)
  / ((inxi baseWthX / Int 2) * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc)),
  
  foldlSP [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "a general", phrase equation, S "for the constant", getS normToShear,
  S "is developed in", eqN 15 `sC` S "also found in", acroIM 2],
  --NOTE: "Taking this with that and the assumption of _
  --to get equation #" pattern
  
  EqnBlock $
  inxi normToShear := sum1toN
  (inxi baseWthX * (Grouping (inxi intNormForce + inxiM1 intNormForce) +
  Grouping (inxi watrForce + inxiM1 watrForce)) * tan(inxi baseAngle) +
  inxi midpntHght * (C earthqkLoadFctr * inxi slcWght -
  Int 2 * inxi surfHydroForce * sin(inxi surfAngle) -
  Int 2 * inxi surfLoad * sin(inxi impLoadAngle))) / 
  sum1toN
  (inxi baseWthX * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc)),
  
  foldlSP [eqN 15, S "for", getS normToShear `sC`
  S "is a function of the unknown", getTandS intNormForce, acroIM 3]

  ]

intrSlcDerivation = [

  foldlSP [S "Taking the", S "normal force equilibrium" `sOf` acroGD 1,
  S "with the", S "effective stress", phrase definition, S "from", acroT 4,
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  S "that", E (inxi totNrmForce := inxi nrmFSubWat - inxi baseHydroForce) `sC`
  S "and the assumption of", acroGD 5, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16],
  
  EqnBlock $
  inxi nrmFSubWat := eqlExpr cos sin (\x y -> x -
  C normToShear * inxiM1 scalFunc * inxiM1 intNormForce + 
  C normToShear * inxi scalFunc * inxi intNormForce + y)
  - (inxi baseHydroForce),
  
  foldlSP [S "Taking the", S "base shear force equilibrium" `sOf`
  acroGD 2, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", acroGD 4 `sAnd`
  S "the assumption of", acroGD 5 `sC`
  S "the equilibrium", phrase equation,
  S "can be rewritten as", eqN 17],
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  
  EqnBlock $
  ((inxi totNrmForce) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)) / (C fs) :=
  --FIXME: pull the left side of this from GD4
  eqlExpr sin cos (\x y -> x - C normToShear * inxiM1 scalFunc *
  inxiM1 intNormForce + C normToShear * inxi scalFunc * inxi intNormForce + y),
  
  foldlSP [S "Substituting the", phrase equation, S "for", getS nrmFSubWat,
  S "from", eqN 16, S "into", eqN 17, S "and rearranging results in", eqN 18],

  EqnBlock $
  (inxi intNormForce) * (((C normToShear)*(inxi scalFunc) *
  cos (inxi baseAngle) - sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((C normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (C fs)) := (inxiM1 intNormForce) *
  (((C normToShear)*(inxiM1 scalFunc) * cos (inxi baseAngle)
  - sin (inxi baseAngle)) * tan (inxi fricAngle) - ((C normToShear) *
  (inxiM1 scalFunc) * sin (inxi baseAngle) - cos (inxi baseAngle)) *
  (C fs)) + (C fs) * (inxi shearFNoIntsl) - (inxi shearRNoIntsl),
  
  foldlSP [S "Where", getS shearRNoIntsl `sAnd` getS shearFNoIntsl,
  S "are the resistive and mobile shear of the slice" `sC`
  S wiif, getS intNormForce `sAnd` getS intShrForce `sC`
  S "as defined in", acroDD 10 `sAnd` acroDD 11,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", acroIM 3],
  
  EqnBlock $
  (inxi shrResC) := ((C normToShear)*(inxi scalFunc) * cos (inxi baseAngle) -
  sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((C normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (C fs),
  -- FIXME: index everything here and add "Where i is the local
  -- slice of mass for 1 :<= i :<= n-1"
  EqnBlock $
  (inxi mobShrC) := ((C normToShear)*(inxi scalFunc) *
  cos (inxiP1 baseAngle) - sin (inxiP1 baseAngle)) *
  tan (inxi fricAngle) - ((C normToShear)*(inxi scalFunc) *
  sin (inxiP1 baseAngle) - cos (inxiP1 baseAngle)) * (C fs),
  
  EqnBlock $
  (inxi intNormForce) := (inx mobShrC (-1) * inx intNormForce (-1) +
  C fs * inxi shearFNoIntsl - inxi shearRNoIntsl) / inxi shrResC,
  
  fUnknowns]

rigDisDerivation = [
  
  foldlSP [S "Using the net force-displacement equilibrium",
  phrase equation, S "of a slice from", acroDD 13, S "with", plural definition
  `ofThe` S "stiffness matrices", S "from", acroDD 12, S "and the force", 
  plural definition, S "from", acroGD 7 , S "a broken down force displacement",
  S "equilibrium", phrase equation +:+. S "can be derived", eqN 22,
  S "gives the broken down", phrase equation, S "in the", getS xi,
  S "direction" `sC` S "and", eqN 23, S "gives the broken down",
  phrase equation, S "in the", getS yi, S "direction"],

  EqnBlock fDisEq_rel,
  
  foldlSP [S "Using the known input assumption of", acroA 2 `sC`
  S "the force variable", plural definition, S "of", acroDD 1, S "to",
  acroDD 8, S "on left side" `ofThe` plural equation,
  S "can be solved for. The only unknown in the variables to solve",
  S "for the stiffness values from", acroDD 14 +:+. 
  S "is the displacements", S "Therefore taking the", phrase equation, 
  S "from each slice a set of", E $ 2 * C numbSlices, plural equation
  `sC` S "with", E $ 2 * C numbSlices, S "unknown displacements in the", 
  getS xi `sAnd` getS yi, S "directions of each slice can be derived.",
  S "Solutions for the displacements of each slice can then be found.",
  S "The use of displacement in", phrase definition `ofThe`
  S "stiffness values makes the", phrase equation, S "implicit, which means",
  S "an iterative solution method, with an initial guess for the",
  S "displacements in the stiffness", plural value, S "is required"]

  ]

rigFoSDerivation = [
  foldlSP [S "RFEM analysis can also be used to calculate the",
  phrase fs, S "for the", phrase slope +:+. S "For a slice element",
  getS index, S "the displacements", getS dx_i `sAnd` getS dy_i `sC` 
  S "are solved from the system of", plural equation, S "in" +:+.
  acroIM 4, S "The", phrase definition, S "of", getS rotatedDispl,
  S "as rotation" `ofThe` S "displacement vector", getS genDisplace,
  S "is seen in" +:+. acroGD 9, S "This is used to find",
  plural displacement `ofThe` S "slice parallel to", S "base" `ofThe`
  S "slice", getS shrDispl `sIn` eqN 24, S "and normal to", 
  S "base" `ofThe` S "slice", getS nrmDispl, S "in", eqN 25],
  
  EqnBlock $ inxi shrDispl := cos(inxi baseAngle) * inxi dx_i +
  sin(inxi baseAngle) * inxi dy_i,

  EqnBlock $ inxi nrmDispl := Neg (sin(inxi baseAngle)) * inxi dx_i +
    sin(inxi baseAngle) * inxi dy_i,
  
  foldlSP [S "With the", phrase definition, S "of normal stiffness from",
  acroDD 14, --FIXME: grab nrmStiffBase's term name?
  S "to find", S "normal stiffness" `ofThe` S "base", getS nrmStiffBase,
  S "and the now known base displacement perpendicular to the surface",
  getS nrmDispl, S "from", eqN 25, S "the normal base stress",
  S "can be calculated from the force-displacement relationship of" +:+.
  acroT 5, S "Stress", getS normStress `sIs` S "used in place of",
  getTandS genForce, --FIXME: use getTandS
  S "as the stiffness hasn't been normalized for" +:+.
  (S "length" `ofThe` S "base"), S "Results" `sIn` eqN 26],
  --FIXME: grammar

  EqnBlock $
  inxi normStress := inxi nrmStiffBase * inxi nrmDispl, --FIXME: index
  
  foldlSP [S "The resistive shear to calculate the", getTandS fs,
  S "is found from the Mohr Coulomb resistive strength of soil in", acroT 3,
  S "Using the", getTandS normStress, S "from", eqN 26, S "as the stress" `sC`
  (S "resistive shear" `ofThe` S "slice"), S "can be calculated from", eqN 27],
  
  EqnBlock $
  inxi mobStress := inxi cohesion - inxi normStress * tan(inxi fricAngle),
  --FIXME: index and prime
  
  foldlSP [S "Previously", phrase value `ofThe` getTandS shrStiffBase,
  S "as seen in", eqN 28, S "was unsolvable because the", getTandS normStress,
  S "was unknown. With the", phrase definition, S "of", getS normStress,
  S "from", eqN 26, S "and the", phrase definition,
  S "of displacement shear to the base", getS shrDispl, S "from",
  eqN 25 `sC` S "the value of", getS shrStiffBase, S "becomes solvable"],
  
  EqnBlock $
  inxi shrStiffBase := inxi intNormForce / (2 * (1 + inxi poissnsRatio)) *
  (Dbl 0.1 / inxi baseWthX) +
  (inxi cohesion - inxi normStress * tan(inxi fricAngle)) /
  (abs (inxi shrDispl) + C constant_a),
  
  foldlSP [S "With", getTandS shrStiffBase, S "calculated in", eqN 28,
  S "and shear displacement", getS shrDispl, S "calculated in", eqN 24,
  --FIXME: grab term too once we have a displacement modifier
  S "values now known the", phrase shrStress, shrStress ^. defn,
  getS shrStress, S "can be calculated using", acroT 5 `sC`
  S "as done in" +:+. eqN 29, S "Again, stress", getS shrStress,
  S "is used in place of force", getS genForce, --FIXME: grab term
  S "as the stiffness has not been normalized for",
  S "length" `ofThe` S "base"],
  
  EqnBlock $
  inxi shrStress := inxi shrStiffBase * inxi shrDispl,
  
  foldlSP [S "The", getTDS shrStress, S "acts as the mobile shear",
  S "acting on the base. Using the", phrase definition, titleize fs,
  phrase equation, S "from", acroT 1 `sC` S "with the", 
  plural definition, S "of resistive shear strength of a slice",
  getS mobStress, S "from", eqN 27, S "and", getTandS shrStress,
  S "from", eqN 29, S "the", getTandS fsloc,
  S "can be found from as seen in", eqN 30 `sAnd` acroIM 5],
  
  EqnBlock $
  C fsloc := inxi mobStress / inxi shrStress := fosFracLoc,
  
  foldlSP [S "The global", titleize fs, S "is then", S "ratio" `ofThe`
  S "summation of the resistive and mobile shears for each slice" `sC`
  S "with a weighting for" +:+. (S "length" `ofThe` S "slice's base"),
  S "Shown in", eqN 31 `sAnd` acroIM 5],
  
  EqnBlock $
  (C fs) := sum1toN (inxi baseLngth * inxi mobStress) /
  sum1toN (inxi baseLngth * inxi shrStress) := fosFracSum
  ]
