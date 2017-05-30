module Data.Drasil.Units.Physics where


import Language.Drasil.Unit (new_unit, makeDerU)
import Language.Drasil
import Data.Drasil.SI_Units
import Control.Lens ((^.))


accelU, angVelU, angAccelU, momtInertU, momentOfForceU, impulseU, springConstU, torqueU, velU :: DerUChunk

accelU          = new_unit "acceleration"         $ metre /: s_2
angVelU         = new_unit "angular velocity"     $ radian /: second
angAccelU       = new_unit "angular acceleration" $ radian /: s_2
impulseU        = new_unit "impulse"              $ newton *: second
momtInertU      = new_unit "moment of inertia"    $ kilogram *: m_2
momentOfForceU  = new_unit "moment of force"      $ newton *: metre
springConstU    = new_unit "spring constant"      $ newton /: metre
torqueU         = new_unit "torque"               $ newton *: metre
velU            = new_unit "velocity"             $ metre /: second

gravConstU :: DerUChunk

gravConstU = makeDerU (dcc "gravConstU" (cn "gravitational constant")
  "universal gravitational constant") $
   USynonym (UDiv (m_3 ^. usymb) (UProd [kilogram ^. usymb, s_2 ^. usymb]))
