module Drasil.GlassBR.Reqs where

import Language.Drasil

import Drasil.GlassBR.Modules
import Data.Drasil.Modules
import Drasil.GlassBR.Concepts

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6]

r1,r2,r3,r4,r5,r6 :: ReqChunk

-- FIXME: having id "" and term "" is completely bogus, and should not
-- be allowed.  This implicitly says that something here does not make sense.
r1 = ReqChunk (nw emptyN) [mod_hw, mod_inputf, mod_inputp, (mod_ctrl glassBRProg [mod_inputf,
                                                                                  mod_inputp,
                                                                                  mod_inputc,
                                                                                  mod_derivedv, 
                                                                                  mod_calc,
                                                                                  mod_interp,
                                                                                  mod_outputf])]
r2 = ReqChunk (nw emptyN) [mod_inputf, mod_inputp]
r3 = ReqChunk (nw emptyN) [mod_inputc]
r4 = ReqChunk (nw emptyN) [mod_outputf]
r5 = ReqChunk (nw emptyN) [mod_outputf, mod_calc]
r6 = ReqChunk (nw emptyN) [mod_outputf]

emptyN :: NPNC
emptyN = npnc "" (cn "")
