{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPendulum.Body where

import Language.Drasil hiding (organization, section)
import Theory.Drasil (TheoryModel)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration, piSys)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb, _folderPath)
import Utils.Drasil
import Utils.Drasil.Concepts
import Data.Drasil.Concepts.Education (educon)
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.People (olu)
import Data.Drasil.SI_Units (metre, second, newton, kilogram, degree, radian, hertz)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Concepts.Software (program, errMsg)
import Data.Drasil.Concepts.Physics (gravity, physicCon, physicCon', pendulum, twoD, motion)
import Data.Drasil.Theories.Physics (newtonSL, accelerationTM, velocityTM, newtonSLR)
import Data.Drasil.TheoryConcepts (inModel)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.PhysicalProperties (mass, len, physicalcon)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Documentation (problem, srsDomains, doccon, doccon', analysis)
import Data.Drasil.Concepts.Computation (compcon, inValue, algorithm)
import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation),
  DocSection(..),
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(..), IntroSub(IPurpose, IScope, IChar, IOrgSec), ProblemDescription(PDProg), PDSub(..),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SRSDecl,
  SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  TConvention(..), TSIntro(..), TraceabilitySec(TraceabilityProg), GSDSec(..), GSDSub(..),
  Verbosity(Verbose), intro, mkDoc, traceMatStandard, tsymb, purpDoc, fillcdbSRS)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Math (mathcon, cartesian, mathcon')
import Data.Drasil.Quantities.Math (unitVect, unitVectj)
import Data.Drasil.Domains (physics)

import Drasil.DblPendulum.Body (justification, charsOfReader, organizationOfDocumentsIntro,
  physSystParts, sysCtxIntro, sysCtxDesc, sysCtxFig1,
  sysCtxList, userCharacteristicsIntro)
import Drasil.DblPendulum.Concepts (concepts, pendMotion)
import Drasil.DblPendulum.Requirements (nonFuncReqs)
import Drasil.DblPendulum.Unitals (symbols, acronyms)

import Drasil.SglPendulum.Figures (figMotion)
import Drasil.SglPendulum.Assumptions (assumptions)
import Drasil.SglPendulum.Goals (goals, goalsInputs)
import Drasil.SglPendulum.DataDefs (dataDefs)
import Drasil.SglPendulum.IMods (iMods)
import Drasil.SglPendulum.GenDefs (genDefns)
import Drasil.SglPendulum.Unitals (inputs, outputs,
  inConstraints, outConstraints)
import Drasil.SglPendulum.Requirements (funcReqs)
import Drasil.SglPendulum.References (citations)


srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

resourcePath :: String
resourcePath = "../../../datafiles/SglPendulum/"

mkSRS :: SRSDecl
mkSRS = [TableOfContents, -- This creates the Table of Contents
  RefSec $      --This creates the Reference section of the SRS
    RefProg intro      -- This add the introduction blob to the reference section  
      [ TUnits         -- Adds table of unit section with a table frame
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits] -- Adds table of symbol section with a table frame
      --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
      , TAandA         -- Add table of abbreviation and acronym section
      ],
  IntroSec $
    IntroProg justification (phrase progName)
      [IPurpose $ purpDoc progName Verbose,
       IScope scope,
       IChar [] charsOfReader [],
       IOrgSec organizationOfDocumentsIntro inModel (SRS.inModel [] []) EmptyS],
  GSDSec $ 
    GSDProg [
      SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
      UsrChars [userCharacteristicsIntro], 
      SystCons [] []],                            
  SSDSec $ 
    SSDProg
      [ SSDProblem $ PDProg prob []                --  This adds a is used to define the problem your system will solve
        [ TermsAndDefs Nothing terms               -- This is used to define the terms to be defined in terminology sub section
      , PhySysDesc progName physSystParts figMotion [] -- This defines the Physicalsystem sub-section, define the parts
                                                          -- of the system using physSysParts, figMotion is a function in figures for the image
      , Goals goalsInputs] -- This adds a goals section and goals input is defined for the preample of the goal.
      , SSDSolChSpec $ SCSProg --This creates the solution characteristics section with a preamble
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
       ]
     ],
  ReqrmntSec $ ReqsProg
    [ FReqsSub EmptyS []
    , NonFReqsSub
    ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
     AuxConsProg progName [],  --Adds Auxilliary constraint section
  Bibliography                    -- Adds reference section
  ]

-- Folder name. Used in traceability graphs.
directoryName :: FilePath
directoryName = "SglPendulum"

progName :: CI
progName = commonIdeaWithDict "pendulumTitle" (pn "Pendulum") "SglPendulum" [physics]

si :: SystemInformation
si = SI {
  _sys         = progName, 
  _kind        = Doc.srs,
  _authors     = [olu],
  _purpose     = purpDoc progName Verbose,
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _folderPath  = directoryName,
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = inConstraints,
  _constants   = [] :: [QDefinition],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb (map qw iMods ++ map qw symbols)
  (nw newtonSLR : nw progName : nw mass : nw len : nw kilogram : nw inValue : nw newton : nw degree : nw radian
    : nw unitVect : nw unitVectj : [nw errMsg, nw program] ++ map nw symbols ++
   map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw mathcon ++ map nw mathcon' ++ map nw physicCon' ++
   map nw physicscon ++ concepts ++ map nw physicalcon ++ map nw acronyms ++ map nw symbols ++ map nw [metre, hertz] ++
   [nw algorithm] ++ map nw compcon ++ map nw educon ++ map nw prodtcon)
  (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz]) dataDefs
  iMods genDefns tMods concIns [] [] ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbols) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ funcReqs ++ nonFuncReqs
-- ++ likelyChgs ++ unlikelyChgs


------------------------------
-- Section : INTRODUCTION --
------------------------------

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope :: Sentence
scope = foldlSent_ [phraseNP (NP.the (analysis `ofA` twoD)), 
  sParen (getAcc twoD), phrase pendMotion, phrase problem,
                   S "with various initial conditions"]

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.4 : Organization of Documents --
-------------------------------------

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------
-- Description of Genreal System automatically generated in GSDProg

--------------------------
-- 3.1 : System Context --
--------------------------

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

-------------------------------
-- 3.3 : System Constraints  --
-------------------------------
-- System Constraints automatically generated in SystCons


--------------------------------------------
-- Section 4: Specific System Description --
--------------------------------------------
-- Description of Specific System automatically generated in SSDProg

-------------------------------
-- 4.1 : System Constraints  --
-------------------------------
prob :: Sentence
prob = foldlSent_ [ S "efficiently and correctly to predict the", phraseNP (motion `ofA`  
                   pendulum)]

---------------------------------
-- 4.1.1 Terminology and Definitions --
---------------------------------
terms :: [ConceptChunk]
terms = [gravity, cartesian]

tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM, newtonSL, newtonSLR]

-----------------------------------
-- 4.1.2 Physical System Description --
-----------------------------------

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

-------------------------
-- 4.2.1 : Assumptions --
-------------------------
-- Assumptions defined in Assumptions

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------
-- Theoretical Models defined in TMs

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------
-- General Definitions defined in GDs

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------
-- Data Definitions defined in DDs

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------
-- Instance Models defined in IMs

-----------------------------
-- 4.2.6 : Data Constraints --
-----------------------------
-- Data Constraints defined in Constraints

-----------------------------
-- 4.2.7 : Properties of a Correct Solution --
-----------------------------
-- Properties of a Correct Solution defined in CorrSolnPpties

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------
-- in Requirements.hs

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

--------------------------------
-- SECTION 6b : UNLIKELY CHANGES --
--------------------------------

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------

----------------------------
-- Section 9 : References --
----------------------------