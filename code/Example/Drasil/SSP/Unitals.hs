module Drasil.SSP.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Units.SolidMechanics (stiffness3D)
import Data.Drasil.Quantities.Physics as QP (force, pressure)
import Data.Drasil.Quantities.SolidMechanics as SM (nrmStrss, elastMod, poissnsR, stffness)
import Data.Drasil.Units.Physics
import Drasil.SSP.Defs (fs_concept)

sspSymbols :: [CQSWrapper]
sspSymbols = (map cqs sspInputs) ++ (map cqs sspOutputs) ++
  (map cqs sspUnits) ++ (map cqs sspUnitless) 

---------------------------
-- Imported UnitalChunks --
---------------------------
{-
SM.mobShear, SM.shearRes <- currently not used
SM.poissnsR, SM.elastMod <- Used to make UncertQ
-}
normStress  = SM.nrmStrss
genForce = uc QP.force cF newton
genPressure = QP.pressure
genStffness = SM.stffness
{-must import from Concept.Physics since Quantities.Physics has Force as a vector-}

-------------
-- HELPERS --
-------------
fsi, fisi, wiif, wla, smsi :: String
fsi   = "for slice index i"
fisi  = "for interslice index i"
wiif  = "without the influence of interslice forces"
wla   = "without length adjustment"
smsi  = "refers to either slice i midpoint, or slice interface i"

--------------------------------
-- START OF CONSTRAINEDCHUNKS --
--------------------------------

sspConstrained :: [ConstrWrapper]
sspConstrained = map cnstrw sspInputs ++ map cnstrw sspOutputs

sspInputs :: [UncertQ]
sspInputs  = [elasticMod, cohesion, poissnsRatio, fricAngle, dryWeight,
              satWeight, waterWeight, constant_a, constant_A, constant_K]

sspOutputs :: [ConstrConcept]
sspOutputs = [fs, coords, dx_i, dy_i]

gtZeroConstr :: [Constraint] --FIXME: move this somewhere in Data?
gtZeroConstr = [physc $ (:<) (Int 0)]

monotonicIn :: [Constraint]  --FIXME: Move this? Re word?
monotonicIn = [physc $ \c ->
  State [Forall c, Forall $ [V "x1", V "x2"] `IsIn` Real] (V "x1" :< V "x2" :=> V "y1" :< V "y2")]

defultUncrt :: Double
defultUncrt = 0.1

elasticMod, cohesion, poissnsRatio, fricAngle, dryWeight, satWeight,
  waterWeight, constant_a, constant_A, constant_K :: UncertQ
  
fs, coords, dx_i, dy_i :: ConstrConcept

{-Intput Variables-}
--FIXME: add (x,y) when we can index or make related unitals

elasticMod = uq (constrained' SM.elastMod gtZeroConstr (Dbl 15000)) defultUncrt

cohesion     = uqc "c'" (cn $ "effective cohesion")
  "internal pressure that sticks particles of soil together"
  (prime $ Atomic "c") pascal Real gtZeroConstr (Dbl 10) defultUncrt

poissnsRatio = uq (constrained' SM.poissnsR
  [physc $ \c -> (Int 0) :< c :< (Int 1)] (Dbl 0.4)) defultUncrt

fricAngle    = uqc "varphi'" (cn $ "effective angle of friction")
  ("The angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime $ Greek Phi_V) degree Real [physc $ \c -> (Int 0) :< c :< (Int 90)]
  (Dbl 25) defultUncrt

dryWeight   = uqc "gamma" (cn $ "dry unit weight")
  "The weight of a dry soil/ground layer divided by the volume of the layer."
  (Greek Gamma_L) specific_weight Real gtZeroConstr
  (Dbl 20) defultUncrt

satWeight   = uqc "gamma_sat" (cn $ "saturated unit weight")
  "The weight of saturated soil/ground layer divided by the volume of the layer."
  (sub (Greek Gamma_L) (Atomic "Sat")) specific_weight Real gtZeroConstr
  (Dbl 20) defultUncrt

waterWeight = uqc "gamma_w" (cn $ "unit weight of water")
  "The weight of one cubic meter of water."
  (sub (Greek Gamma_L) lW) specific_weight Real gtZeroConstr
  (Dbl 9.8) defultUncrt
  
constant_a  = uqc "a" (cn "constant") "FIXME: missing discription"
  lA metre Real [] (Dbl 0) defultUncrt
  
constant_A  = uqc "A" (cn "constant") "FIXME: missing discription"
  cA metre Real [] (Dbl 0) defultUncrt
  
constant_K  = uqc "kappa" (cn "constant") "FIXME: missing discription"
  (Greek Kappa_L) pascal Real [] (Dbl 0) defultUncrt

{-Output Variables-} --FIXME: See if there should be typical values
fs          = constrained' (cvR fs_concept (Atomic "FS")) gtZeroConstr (Dbl 1)

coords      = cuc' "(x,y)"
  (cn $ "cartesian position coordinates" )
  ("y is considered parallel to the direction of the force of " ++
  "gravity and x is considered perpendicular to y")
  (Atomic "(x,y)") metre Real monotonicIn (Dbl 1)

dx_i        = cuc' "dx_i" (cn $ "displacement") ("in the x-ordinate direction " ++ fsi)
  (Concat [Greek Delta_L, Atomic "x"]) metre Real [] (Dbl 1)

dy_i        = cuc' "dy_i" (cn $ "displacement") ("in the y-ordinate direction " ++ fsi)
  (Concat [Greek Delta_L, Atomic "y"]) metre Real [] (Dbl 1)

---------------------------
-- START OF UNITALCHUNKS --
---------------------------

sspUnits :: [UCWrapper]
sspUnits = map ucw [normStress, genPressure, normFunc, shearFunc,
  waterHght, slopeHght, slipHght, xi, critCoords, slopeDist, slipDist,
  mobShrI, shrResI, shearFNoIntsl, shearRNoIntsl, slcWght, watrForce,
  watrForceDif, intShrForce, baseHydroForce, surfHydroForce,
  totNrmForce, nrmFSubWat, nrmFNoIntsl, surfLoad, baseAngle, surfAngle,
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, genForce,
  momntOfBdy, genDisplace, genStffness, shrStiffIntsl, shrStiffBase,
  nrmStiffIntsl, nrmStiffBase, shrStiffRes, nrmStiffRes, shrDispl,
  nrmDispl, porePressure, elmNrmDispl, elmPrllDispl, sliceHght,
  mobShrC, shrResC, rotatedDispl, intNormForce, shrStress, mobStress]

normStress, genPressure, normFunc, shearFunc, slopeDist, slipDist, genStffness,
  waterHght, slopeHght, slipHght, xi, critCoords, mobShrI, sliceHght,
  shearFNoIntsl, shearRNoIntsl, slcWght, watrForce, watrForceDif, shrResI,
  intShrForce, baseHydroForce, surfHydroForce, totNrmForce, nrmFSubWat,
  nrmFNoIntsl, surfLoad, baseAngle, surfAngle, impLoadAngle, baseWthX,
  baseLngth, surfLngth, midpntHght, genForce, momntOfBdy, genDisplace,
  shrStiffIntsl, shrStiffBase, nrmStiffIntsl, nrmStiffBase, shrStiffRes,
  nrmStiffRes, shrDispl, nrmDispl, porePressure, elmNrmDispl, mobStress,
  elmPrllDispl, mobShrC, shrResC, rotatedDispl, intNormForce, shrStress :: UnitalChunk
  
{-FIXME: Many of these need to be split into term, defn pairs as
         their defns are mixed into the terms.-}

intNormForce = uc' "E_i" (cn $ "interslice normal force")
  ("exerted between adjacent slices " ++ fisi)
  (cE) newton

waterHght   = uc' "y_wt,i"
  (cn $ "y ordinate")
  ("height of the water table at i, " ++ smsi)
  (sub lY (Atomic "wt")) metre

slopeHght   = uc' "y_us,i" (cn $ "y ordinate")
  ("height of the top of the slope at i, " ++ smsi)
  (sub lY (Atomic "us")) metre

slipHght    = uc' "y_slip,i" (cn $ "y ordinate")
  ("height of the slip surface at i, " ++ smsi)
  (sub lY (Atomic "slip")) metre

slopeDist   = uc' "x_us,i" (cn $ "x ordinate")
  ("distance of the edge of the slope at i, " ++ smsi)
  (sub lX (Atomic "us")) metre 

slipDist    = uc' "x_slip,i" (cn $ "x ordinate")
  ("distance of the slip surface at i, " ++ smsi)
  (sub lX (Atomic "slip")) metre

xi          = uc' "x_i"
  (cn $ "x ordinate")
  smsi
  lX metre

critCoords  = uc' "(xcs,ycs)" (cn $ "the set of x and y coordinates")
  "describe the vertices of the critical slip surface"
  (Concat [sub (Atomic "{x") (Atomic "cs"), sub (Atomic ",y") (Atomic "cs"),
  Atomic "}"]) metre

mobShrI     = uc' "S_i" (cn $ "mobilized shear force")
  fsi
  (sub cS lI) newton

shrResI     = uc' "P_i" (cn $ "resistive shear force") ("Mohr Coulomb " ++
  "frictional force that describes the limit of mobilized shear force the " ++
  "slice i can withstand before failure")
  (cP) newton
  
mobShrC     = uc' "Psi" (cn $ "constant") ("converts mobile shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces")
  (Greek Psi) newton

shrResC     = uc' "Phi" (cn $ "constant") ("converts resistive shear " ++ 
  wiif ++ ", to a calculation considering the interslice forces")
  (Greek Phi) newton

shearFNoIntsl = uc' "T_i"
  (cn $ "mobilized shear force") (wiif ++ " " ++ fsi)
  cT newton

shearRNoIntsl = uc' "R_i"
  (cn $ "resistive shear force") (wiif ++ " " ++ fsi)
  (cR) newton

slcWght     = uc' "W_i" (cn $ "weight") ("downward force caused by gravity on slice i")
  (cW) newton

watrForce    = uc' "H_i" (cn $ "interslice water force") ("exerted in the " ++
  "x-ordinate direction between adjacent slices " ++ fisi)
  (cH) newton

watrForceDif = uc' "dH_i" (cn $ "difference between interslice forces") ("exerted in the " ++
  "x-ordinate direction between adjacent slices " ++ fisi)
  (Concat [Greek Delta, cH]) newton

intShrForce = uc' "X_i" (cn $ "interslice shear force") 
  ("exerted between adjacent slices " ++ fisi)
  (cX) newton

baseHydroForce = uc' "U_b,i" (cn $ "base hydrostatic force")
  ("from water pressure within the slice " ++ fsi)
  (sub cU (Atomic "b")) newton

surfHydroForce = uc' "U_t,i" (cn $ "surface hydrostatic force")
  ("from water pressure acting into the slice from standing water on the slope surface " ++ fsi)
  (sub cU (Atomic "t")) newton

totNrmForce = uc' "N_i" (cn $ "normal force") ("total reactive force " ++
  "for a soil surface subject to a body resting on it")
  cN newton

nrmFSubWat = uc' "N'_i" (cn $ "effective normal force") ("for a soil surface, " ++
  "subtracting pore water reactive force from total reactive force")
  (prime $ Atomic "N") newton

nrmFNoIntsl = uc' "N*_i" (cn $ "effective normal force") ("for a soil surface, " ++
  wiif)
  (Atomic "N*") newton

surfLoad    = uc' "Q_i" (cn $ "imposed surface load") 
  "a downward force acting into the surface from midpoint of slice i"
  (cQ) newton

baseAngle   = uc' "alpha_i" (cn $ "angle") ("base of the mass relative to the horizontal " ++ fsi)
  (Greek Alpha_L) degree

surfAngle   = uc' "beta_i" (cn $ "angle") ("surface of the mass relative to the horizontal " ++ fsi)
  (Greek Beta_L) degree

impLoadAngle = uc' "omega_i" (cn $ "angle")
  ("of imposed surface load acting into the surface relative to the vertical " ++ fsi)
  (Greek Omega_L) degree

baseWthX    = uc' "b_i" (cn $ "base width of a slice")
  ("in the x-ordinate direction only " ++ fsi)
  (lB) metre

baseLngth   = uc' "l_b,i" (cn $ "total base length of a slice") fsi
  (sub (Greek Ell) (Atomic "b")) metre

surfLngth   = uc' "l_s,i" (cn $ "length of an interslice surface")
  ("from slip base to slope surface in a vertical line from an interslice vertex " ++ fisi)
  (sub (Greek Ell) (Atomic "s")) metre

midpntHght  = uc' "h_i" (cn $ "midpoint height")
  ("distance from the slip base to the slope surface in a vertical line from the midpoint of the slice " ++ fsi)
  (lH) metre

momntOfBdy  = uc' "M" (cn $ "moment of a body") ("assumed 2D allowing a scalar")
  cM momentOfForceU --FIXME: move in concepts.physics ?

genDisplace = uc' "genDisplace" (cn $ "displacement") "generic displacement of a body"
  (Greek Delta_L) metre

shrStiffIntsl = uc' "K_st,i" (cn $ "shear stiffness")
  ("for interslice surface, " ++ wla ++ " " ++ fisi)
  (sub cK (Atomic "st")) stiffness3D

shrStiffBase  = uc' "K_bt,i" (cn $ "shear stiffness") 
  ("for a slice base surface, " ++ wla ++ " " ++ fsi)
  (sub cK (Atomic "bt")) stiffness3D

nrmStiffIntsl = uc' "K_sn,i" (cn $ "normal stiffness")
  ("for an interslice surface, " ++ wla ++ " " ++ fisi)
  (sub cK (Atomic "sn")) stiffness3D

nrmStiffBase = uc' "K_bn,i" (cn $ "normal stiffness") 
  ("for a slice base surface, " ++ wla ++ " " ++ fsi)
  (sub cK (Atomic "bn")) stiffness3D

shrStiffRes  = uc' "K_tr" (cn $ "shear stiffness")
  "residual strength"
  (sub cK (Atomic "tr")) stiffness3D

nrmStiffRes  = uc' "K_no" (cn $ "normal stiffness")
  "residual strength"
  (sub cK (Atomic "no")) stiffness3D

shrDispl = uc' "du_i" (cn $ "displacement")
  ("shear displacement " ++ fsi)
  (Concat [Greek Delta_L, Atomic "u"]) metre

nrmDispl = uc' "dv_i" (cn $ "displacement")
  ("normal displacement " ++ fsi)
  (Concat [Greek Delta_L, Atomic "v"]) metre
  
elmNrmDispl  = uc' "dt_i" (cn $ "displacement")
  ("for the element normal to the surface " ++ fsi)
  (Concat [Greek Delta_L, Atomic "t"]) metre
  
elmPrllDispl = uc' "dn_i" (cn $ "displacement")
  ("for the element parallel to the surface " ++ fsi)
  (Concat [Greek Delta_L, Atomic "n"]) metre

porePressure = uc' "mu" (cn "pore pressure") ("from water within the soil")
  (Greek Mu_L) pascal

rotatedDispl = uc' "varepsilon_i" (cn "displacement") ("in rotated coordinate system")
  (Greek Epsilon_V) metre
  
shrStress    = uc' "tau_i" (cn "resistive shear stress") ("acting on the base of a slice")
  (Greek Tau_L) pascal
  
mobStress    = uc' "s_i" (cn "mobilized shear stress") ("acting on the base of a slice")
  (lS) pascal

sliceHght    = uc' "z_i" (cn "center of slice height") ("the distance from the lowest part " ++
  "of the slice to the height of the centers of slice") (lZ) metre

normFunc     = uc' "C1_i" (cn "interslice normal force function") ("FIXME: missing discription")
  (Concat [cC, Atomic "1"]) momentOfForceU
  
shearFunc    = uc' "C2_i" (cn "interslice shear force function") ("FIXME: missing discription")
  (Concat [cC, Atomic "2"]) momentOfForceU  
  
----------------------
-- Unitless Symbols --
----------------------

sspUnitless :: [ConVar]
sspUnitless = [earthqkLoadFctr, normToShear,scalFunc,
  numbSlices, minFunction, fsloc, index, varblU, varblV]

earthqkLoadFctr, normToShear, scalFunc,
  numbSlices, minFunction, fsloc, index, varblU, varblV :: ConVar

earthqkLoadFctr = cvR (dcc "K_c" (nounPhraseSP $ "earthquake load factor") ("proportionality " ++
  "factor of force that weight pushes outwards; caused by seismic earth movements")) (sub cK lC)

normToShear = cvR (dcc "lambda" (nounPhraseSP $ "interslice normal/shear force ratio")
  ("applied to all interslices")) (Greek Lambda_L)

scalFunc    = cvR (dcc "f_i" (nounPhraseSP $ "scaling function") ("magnitude of interslice " ++
  "forces as a function of the x coordinate" ++ fisi ++ "; can be constant or a half-sine"))
  (lF)

numbSlices  = cvRs (dcc "n" (nounPhraseSP "number of slices") "the slip mass has been divided into")
  lN Natural

minFunction = cvR (dcc "Upsilon" (nounPhraseSP "function") ("generic minimization function or algorithm"))
  (Greek Upsilon)

fsloc       = cvR (dcc "FS_loci" (nounPhraseSP "local factor of safety") fsi)
  (sub (Atomic "FS") (Atomic "Loc,i"))

--------------------
-- Index Function --
--------------------

varblU = cvRs (dcc "varblU" (nounPhraseSP "local index")
  ("used as a bound variable index in calculations"))
  lU Natural
varblV = cvRs (dcc "varblV" (nounPhraseSP "local index")
  ("used as a bound variable index in calculations"))
  lV Natural

index       = cvRs (dcc "index" (nounPhraseSP "index") ("used to show a quantity " ++
  "applies to only one slice")) lI Natural

--FIXME: possibly move to Language/Drasil/Expr.hs
indx1 :: (SymbolForm a) => a -> Expr
indx1 a = Index (C a) (Int 1)

indxn :: (SymbolForm a) => a -> Expr
indxn a = Index (C a) (C numbSlices)

inxi, inxiP1, inxiM1 :: SymbolForm e => e -> Expr
inxiP1 e = inx e 1
inxi   e = inx e 0
inxiM1 e = inx e (-1)

inx :: SymbolForm e => e -> Integer -> Expr
inx e n 
  | n < 0     = Index (C e) (C index - Int (-n))
  | n == 0    = Index (C e) (C index)
  | otherwise = Index (C e) (C index + Int n)