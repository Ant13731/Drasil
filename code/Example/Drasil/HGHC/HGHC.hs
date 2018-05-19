module Drasil.HGHC.HGHC (srsBody, thisCode, allSymbols) where

import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.DocumentLanguage

import Drasil.HGHC.HeatTransfer (hghcVars, fp, htOutputs,
  htInputs, symbols, nuclearPhys, hghc)

import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.SpecificSystemDescription (dataDefnF)

import Data.Drasil.SI_Units (si_units)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (srs)
import Data.Drasil.Phrase (for)

thisCode :: CodeSpec
thisCode = codeSpec thisSI []
  
thisSI :: SystemInformation
thisSI = SI {
  _sys = hghc,
  _kind = srs,
  _authors = [spencerSmith],
  _units = si_units,  
  _quants = symbols,
  _concepts = ([] :: [UnitaryConceptDict]),
  _definitions = hghcVars,
  _inputs = htInputs,
  _outputs = htOutputs,
  _defSequence = ([] :: [Block QDefinition]),
  _constraints = ([] :: [ConstrainedChunk]),
  _constants = [],
  _sysinfodb = allSymbols,
  _refdb = rdb [] [] [] [] [] [] -- FIXME?
}

allSymbols :: ChunkDB
allSymbols = cdb symbols (map nw symbols) ([] :: [ConceptChunk]) -- FIXME: Fill in concepts
  si_units
  
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
