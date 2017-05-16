module Drasil.GlassBR.Modules where

import Language.Drasil
import Data.Drasil.Concepts.Software
import Drasil.GlassBR.Concepts

import Data.Drasil.Modules

-- Some of the content below is 'generic' and should be pulled out from here.
-- And the constructors for making 'modules' should be rethought to be more
-- convenient for the most common cases.
modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputc, mod_outputf, 
  mod_derivedv, mod_calc, (mod_ctrl glassBRProg [mod_inputf, mod_inputp, mod_inputc, mod_derivedv, mod_calc,
  mod_interp, mod_outputf]), mod_interpd, mod_sw, mod_interp]

-- input format module
mod_inputf :: ModuleChunk
mod_inputf = makeImpModule modInputFormat
  (S "The format and structure of the input data.")
  glassBRProg
  []
  []
  [mod_hw, mod_inputp]
  (Just mod_behav)

mod_inputp :: ModuleChunk
mod_inputp = makeImpModule modInputParam --FIXME: Plural?
  (S "The format and structure of the input parameters.")
  glassBRProg
  []
  []
  [mod_inputc]
  (Just mod_behav)

-- input constraints module
mod_inputc :: ModuleChunk
mod_inputc = makeImpModule modInputConstraint --FIXME: Plural?
  (S "The constraints on the input data.")
  glassBRProg
  []
  []
  []
  (Just mod_behav)

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = dccWDS "mod_outputf_desc" (cn' "output format")
  (S "Outputs the results of the calculations, including the input " :+:
   S "parameters, the demand, the capacity, the probability of breakage, " :+:
   S "and both safety requirements.")

mod_outputf :: ModuleChunk
mod_outputf = makeImpModule mod_outputf_desc
  (S "The format and structure of the output data.")
  glassBRProg
  []
  []
  [mod_hw, mod_inputp]
  (Just mod_behav)

-- derived values module
mod_derivedv :: ModuleChunk
mod_derivedv = makeImpModule modDerivedVal --FIXME: Plural?
  (S "The transformations from initial inputs to derived quantities.")
  glassBRProg
  []
  []
  [mod_inputp]
  (Just mod_behav)

-- calculations module

-- TODO: Maybe this could be parameterized.
-- FIXME: Should use plural of "calculation"
mod_calc_desc :: ConceptChunk
mod_calc_desc = dccWDS "mod_calc_desc" (cn "calculations")
  (S "Defines the equations for solving for the probability of glass " :+:
   S "breakage, demand, and capacity using the parameters in the input " :+:
   S "parameters module.")

mod_calc :: ModuleChunk
mod_calc = makeImpModule mod_calc_desc
  (S "The equations for predicting the probability of glass breakage, " :+:
   S "capacity, and demand, using the input parameters.")
   glassBRProg
   []
   []
   [mod_inputp]
   (Just mod_behav)

-- interpolation data module

mod_interpd :: ModuleChunk
mod_interpd = makeImpModule modInterpDatum --FIXME: Plural?
  (S "The format and structure of the data used for interpolation.")
   glassBRProg
   []
   []
   []
   (Just mod_behav)

-- interpolation module

mod_interp :: ModuleChunk
mod_interp = makeImpModule modInterpolation
  (S "The interpolation algorithm.")
   glassBRProg
   []
   []
   [mod_interpd]
   (Just mod_sw)
