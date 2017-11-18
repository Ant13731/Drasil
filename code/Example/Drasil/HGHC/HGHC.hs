module Drasil.HGHC.HGHC (srsBody, thisChoices, thisCode, allSymbols) where

import Language.Drasil
import Drasil.DocumentLanguage

import Drasil.HGHC.HeatTransfer (hghcVars, fp, htOutputs,
  htInputs, symbols, nuclearPhys, hghc)

import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.SpecificSystemDescription (dataDefnF)

import Data.Drasil.SI_Units (si_units)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (srs)

thisChoices :: Choices
thisChoices = Choices {
  lang             = [Python, Cpp, CSharp, Java],
  impType          = Program,
  logFile          = "log.txt",
  logging          = LogNone,
  comments         = CommentNone, 
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure   = AsClass
}

thisCode :: CodeSpec
thisCode = codeSpec' thisSI []
  
thisSI :: SystemInformation
thisSI = SI {
  _sys = hghc,
  _kind = srs,
  _authors = [spencerSmith],
  _units = si_units,  
  _quants = symbols,
  _concepts = ([] :: [UCWrapper]),
  _definitions = hghcVars,
  _inputs = htInputs,
  _outputs = htOutputs,
  _defSequence = ([] :: [Block QDefinition]),
  _constraints = ([] :: [ConstrainedChunk]),
  _constants = [],
  _sysinfodb = allSymbols
}

allSymbols :: ChunkDB
allSymbols = cdb symbols (map nw symbols)
  
thisSRS :: DocDesc
thisSRS = RefSec (RefProg intro 
  [TUnits, 
  tsymb [TSPurpose, SymbConvention [Lit (nw nuclearPhys), Manual (nw fp)]]]) : 
--  SSDSec ( SSDProg [ SSDSolChSpec 
--  (SCSProg [DDs [Label, Symbol, Units, DefiningEquation,
--  Description Verbose IncludeUnits (S "")] hghcVars ]) ] ) :
-- Above Data Defs not yet implemented.
  [Verbatim s3]
  
s3 :: Section --, s4 
s3 = dataDefnF EmptyS (map (Definition . Data) hghcVars)
  
srsBody :: Document
srsBody = mkDoc thisSRS (for) thisSI
