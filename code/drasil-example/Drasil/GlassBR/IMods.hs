module Drasil.GlassBR.IMods (symb, iMods, iMods0, iMods1, pbIsSafe, lrIsSafe, instModIntro, pbIsSafeExpr) where

import Control.Lens ( (^.) )

import Prelude hiding (exp)
import Language.Drasil
import Language.Drasil.Printers
import Theory.Drasil (InstanceModel, imNoDeriv, qwC, ModelKinds (OthModel, EquationalModel))
import Utils.Drasil

import Drasil.GlassBR.DataDefs (probOfBreak, calofCapacity, calofDemand,
  pbTolUsr, qRef)
import Drasil.GlassBR.Goals (willBreakGS)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (charWeight, demand, demandq, isSafeLR, isSafePb,
  lRe, pbTol, plateLen, plateWidth, probBr, standOffDist)

import Data.Drasil.Concepts.Documentation (goal)


-- dump common things between a RC and a QD
dumpRCQD :: (HasUID a,
             NamedIdea a,
             Idea a,
             Definition a,
             ConceptDomain a,
             ExprRelat a) 
            => 
            a -> IO ()
dumpRCQD trg = do
  putStr "uid: "
  putStrLn (trg ^. uid)

  putStr "term: "
  -- case (trg ^. term) of
  --   (ProperNoun s _) -> putStrLn s
  --   (CommonNoun s _ _) -> putStrLn s
  --   (Phrase _ _ _ _) -> putStrLn "PHRASE PRINT NOT IMPLEMENTED YET"
  
  putStr "getA: "
  case getA trg of
    Just s -> putStrLn s
    Nothing -> putStrLn "~Nothing~"

  putStr "defn: "
  putStrLn "NOT IMPLEMENTED YET"
  -- TODO: convert `defn trg` into a String?

  putStr "cdom: "
  print $ cdom trg

  putStr "relat: "
  putStrLn "NOT IMPLEMENTED YET"
  -- print $ exprDoc $ relat trg
  -- TODO: convert `relat trg` into a String?

-- QDefinition has a few more things than a  RelationConcept
dumpQD :: QDefinition -> IO ()
dumpQD trg = do
  dumpRCQD trg

  putStr "typ: "
  print $ trg ^. typ

  putStr "symbol: "
  print $ symbolDoc $ symbol trg Equational
  -- TODO: convert `symbol trg` into a String?

  putStr "defnExpr: "
  putStrLn "NOT IMPLEMENTED YET"
  -- TODO: convert `trg ^. defnExpr` into a String?

  putStr "getUnit: "
  case getUnit trg of 
    Nothing -> putStrLn "~Nothing~"
    Just u -> putStrLn "NOT IMPLEMENTED YET"


iMods0 :: [InstanceModel]
iMods0 = [pbIsSafe]

iMods1 :: [InstanceModel]
iMods1 = [lrIsSafe]

iMods :: [InstanceModel]
iMods = [pbIsSafe, lrIsSafe]

symb :: [DefinedQuantityDict]
symb = map dqdWr [plateLen, plateWidth, charWeight, standOffDist] ++ 
  [dqdQd (qw calofDemand) demandq]

{--}

pbIsSafe :: InstanceModel
pbIsSafe = imNoDeriv (EquationalModel pbIsSafeQD) -- (OthModel pbIsSafeRC)
  [qwC probBr $ UpFrom (Exc, 0), qwC pbTol $ UpFrom (Exc, 0)]
  (qw isSafePb) []
  [makeCite astm2009] "isSafePb"
  [pbIsSafeDesc, probBRRef, pbTolUsr]


-- pbIsSafeQD :: QDefinition
-- pbIsSafeQD = ec isSafePb pbIsSafeExpr -- mkQuantDef' isSafePb (nounPhraseSP "Safety Req-Pb") pbIsSafeExpr

-- pbIsSafeQD :: QDefinition
-- pbIsSafeQD = fromEqn' "isSafePb" (nounPhraseSP "Safety Req-Pb") pbIsSafeDesc (eqSymb isSafePb) Boolean pbIsSafeExpr

pbIsSafeQD :: QDefinition
pbIsSafeQD = mkQuantDef' isSafePb (nounPhraseSP "Safety Req-Pb") pbIsSafeExpr

-- mkQuantDef (mkQuant "isSafePb" (nounPhraseSP "probability of glass breakage safety requirement") (eqSymb isSafePb) Boolean Nothing Nothing) pbIsSafeExpr

-- fromEqnSt' "isSafePb" (nounPhraseSP "probability of glass breakage safety requirement") (S "Safety Req-Pb") (symbol isSafePb) Boolean pbIsSafeExpr

pbIsSafeExpr :: Expr
pbIsSafeExpr = sy probBr $< sy pbTol

pbIsSafeRC :: RelationConcept
pbIsSafeRC = makeRC "isSafePb" (nounPhraseSP "Safety Req-Pb")
  EmptyS (sy isSafePb $= pbIsSafeExpr)

{--}

lrIsSafe :: InstanceModel
lrIsSafe = imNoDeriv (OthModel lrIsSafeRC) 
  [qwC lRe $ UpFrom (Exc, 0), qwC demand $ UpFrom (Exc, 0)]
  (qw isSafeLR) []
  [makeCite astm2009] "isSafeLR"
  [lrIsSafeDesc, capRef, qRef] 

lrIsSafeRC :: RelationConcept
lrIsSafeRC = makeRC "safetyReqLR" (nounPhraseSP "Safety Req-LR")
  EmptyS (sy isSafeLR $= sy lRe $> sy demand)
  
iModDesc :: QuantityDict -> Sentence -> Sentence
iModDesc main s = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
    S "considered safe", s `sAre` S "either both True or both False"]
  
-- Intro --

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S willBreakGS, 
  S "is met by", makeRef2S pbIsSafe `sC` makeRef2S lrIsSafe]

-- Notes --

capRef :: Sentence
capRef = definedIn' calofCapacity (S "and is also called capacity")

lrIsSafeDesc :: Sentence
lrIsSafeDesc = iModDesc isSafeLR
  (ch isSafePb +:+ fromSource pbIsSafe `sAnd` ch isSafeLR)

pbIsSafeDesc :: Sentence
pbIsSafeDesc = iModDesc isSafePb
  (ch isSafePb `sAnd` ch isSafePb +:+ fromSource lrIsSafe)

probBRRef :: Sentence
probBRRef = definedIn probOfBreak
