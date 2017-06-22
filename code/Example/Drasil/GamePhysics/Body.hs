module Drasil.GamePhysics.Body where

import Drasil.Template.MG 
import Drasil.Template.DD
import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.SI_Units


import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Software
import Drasil.Sections.TraceabilityMandGs
import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.Physics as QP (time, 
  position, force, velocity, angularVelocity)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody, elasticity, 
  cartesian, friction, rightHand, collision, space, joint)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (ctrOfMass, 
  dimension)
import qualified Data.Drasil.Concepts.Math as CM (equation, surface, ode, 
  constraint)
import Data.Drasil.Utils (foldle, 
  makeTMatrix, itemRefToSent, refFromType, makeListRef, enumSimple, 
  enumBullet, mkRefsList, symbolMapFun, makeConstraint)
import Data.Drasil.SentenceStructures
import Data.Drasil.Software.Products

import qualified Drasil.SRS as SRS
import qualified Drasil.Sections.ReferenceMaterial as RM

import Drasil.GamePhysics.Unitals
import Drasil.GamePhysics.Concepts
import Drasil.GamePhysics.TMods
import Drasil.GamePhysics.IMods
import Drasil.GamePhysics.DataDefs

import Drasil.GamePhysics.Modules
import Drasil.GamePhysics.Changes
import Drasil.GamePhysics.Reqs

import Drasil.DocumentLanguage
import Drasil.Sections.SpecificSystemDescription
import Drasil.Sections.Requirements
import Drasil.Sections.GeneralSystDesc

authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = manyNames authors

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc' mkSRS for' chipmunkSysInfo

mkSRS :: DocDesc 
mkSRS = RefSec (RefProg RM.intro [TUnits, tsymb tableOfSymbols, TAandA]) :
  IntroSec (IntroProg para1_s2_intro (short chipmunk) 
  [IPurpose (para1_s2_1_intro), 
  IScope s2_2_intro_p1 s2_2_intro_p2, 
  IChar (S "rigid body dynamics") (S "high school calculus") (EmptyS), 
  IOrgSec s2_4_intro inModel s4_2_5 EmptyS]) :
  map Verbatim [s3, s4, s5, s6, s7, s8, s9]
    where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder]


    --FIXME: Need to be able to print defn for gravitational constant.

chipmunkSysInfo :: SystemInformation
chipmunkSysInfo = SI chipmunk srs authors chipUnits cpSymbols ([] :: [CQSWrapper])
  cpAcronyms (cpDDefs) (inputSymbols) (outputSymbols) 
  (cpQDefs) --FIXME: All named ideas, not just acronyms.

chipUnits :: [UnitDefn]
chipUnits = map UU [metre, kilogram, second] ++ map UU [newton, radian]

chipmunkMG :: Document
chipmunkMG = mgDoc' chipmunk auths mgBod

mgBod :: [Section]
(mgBod, _) = makeDD likelyChanges unlikelyChanges reqs modules

cpSymbMap :: SymbolMap
cpSymbMap = symbolMap cpSymbols

cpSymMapT :: RelationConcept -> Contents 
cpSymMapT = symbolMapFun cpSymbMap Theory

cpSymMapD :: QDefinition -> Contents 
cpSymMapD = symbolMapFun cpSymbMap Data

--FIXME: The SRS has been partly switched over to the new docLang, so some of
-- the sections below are now redundant. I have not removed them yet, because
-- it makes it easier to test between the two different versions as changes
-- are introduced. Once the SRS is entirely switched to docLang, the redundant
-- sections should be removed.

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section : INTRODUCTION --
------------------------------

para1_s2_intro :: Sentence
para1_s2_intro = foldlSent
  [S "Due to the rising cost of developing", (plural videoGame) `sC` 
  S "developers are looking for ways to save time and money for their" +:+.
  (plural project), S "Using an", (phrase openSource), 
  (phrase physLib),
  S "that is reliable and free will cut down development costs and lead",
  S "to better quality", (plural product_)]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

para1_s2_1_intro :: Sentence
para1_s2_1_intro = foldlSent 
  [S "This", (phrase document), S "descibes the modeling of an",
  (phrase openSource), getAcc twoD, (phrase CP.rigidBody), 
  (phrase physLib), S "used for" +:+. (plural game), S "The", 
  plural goalStmt, S "and", plural thModel, S "used in",
  short chipmunk, S "are provided. This", (phrase document),
  S "is intended to be used as a reference to provide all",
  S "necessary", (phrase information), S "to understand and verify the", 
  (phrase model)]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
s2_2_intro_p1, s2_2_intro_p2 :: Sentence

s2_2_intro_p1 = foldle (+:+) (+:+) EmptyS
  [S "the", (phrase physicalSim),  S "of", (getAcc twoD), 
  (plural CP.rigidBody), S "acted on by forces"] 
  
s2_2_intro_p2 = foldle (+:+) (+:+) EmptyS [S "simulate how these", 
  (plural CP.rigidBody), S "interact with one another"]

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

s2_4_intro :: Sentence

-- FIXME: Citations.
-- FIXME: This can probably be completely pulled out is we decide on the 
--  right convention for the intro across examples.
s2_4_intro = foldlSent 
  [S "The", (phrase organization), S "of this", (phrase document), S "follows the",
  S "template for an", (getAcc srs), S "for scientific",
  S "computing", (phrase software), S "proposed by [1] and [2]"]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

s3 :: Section
s3 = genSysF [] s3_1_intro [] []


--------------------------------
-- 3.1 : User Characteristics --
--------------------------------

s3_1_intro :: Contents
s3_1_intro = foldlSP
  [S "The end user of", (short chipmunk),
  S "should have an understanding of first year programming concepts",
  S "and an understanding of high school", (phrase physics)]

-------------------------------
-- 3.2 : System Constraints  --
-------------------------------


---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

s4 :: Section
s4 = specSysDescr physLib [s4_1, s4_2]

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1 :: Section
s4_1_intro :: Contents

s4_1 = SRS.probDesc [s4_1_intro] [s4_1_1, s4_1_2]

s4_1_intro = foldlSP 
  [S "Creating a gaming", (phrase physLib),
  S "is a difficult task.", (titleize' game), S "need", 
  (plural physLib), S "that simulate", 
  S "objects acting under various", (phrase physical), S "conditions, while", 
  S "simultaneously being fast and efficient enough to work in soft",
  (phrase realtime), S "during the" +:+. (phrase game), S "Developing a", 
  (phrase physLib),
  S "from scratch takes a long period of time and is very costly" `sC`
  S "presenting barriers of entry which make it difficult for", (phrase game),
  S "developers to include", (phrase physics), 
  S "in their" +:+. (plural product_), S "There are a few", S "free,", 
  (phrase openSource), S "and high quality", (plural physLib), 
  S "available to", S "be used for consumer", (plural product_) +:+. 
  (sParen $ makeRef s7), S "By creating a simple, lightweight, fast and portable",
  (getAcc twoD), (phrase CP.rigidBody), (phrase physLib) `sC`
  (phrase game), S "development will be more accessible",
  S "to the masses and higher quality", (plural product_), S "will be produced"]

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1 :: Section
s4_1_1_bullets :: Contents

s4_1_1 = termDefnF EmptyS [s4_1_1_bullets]

s4_1_1_terms :: [ConceptChunk]
s4_1_1_terms = [CP.rigidBody, CP.elasticity, CPP.ctrOfMass, CP.cartesian, CP.rightHand]

s4_1_1_bullets = enumBullet (map (\x -> (at_start x) :+: S ":" +:+ (x ^. defn)) s4_1_1_terms)


-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

s4_1_2 :: Section
s4_1_2_list :: Contents

s4_1_2 = SRS.goalStmt [s4_1_2_list] []

s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, s4_1_2_stmt4 :: [Sentence]
s4_1_2_stmt1 = [S "Given the", (plural physicalProperty) `sC` S "initial", 
  (plural QP.position), S "and",
  (plural QP.velocity) `sC` S "and", (plural QP.force),
  S "applied on a set of", (plural CP.rigidBody) `sC`
  S "determine their new", (plural QP.position), S "and",
  (plural QP.velocity), S "over a period of", (phrase QP.time)]

s4_1_2_stmt2 = [S "Given the", (plural physicalProperty) `sC` S "initial", 
  (plural QM.orientation), S "and", (plural QP.angularVelocity ) `sC`
  S "and", (plural QP.force), S "applied on a set of", 
  (plural CP.rigidBody) `sC` S "determine their new",
  (plural QM.orientation), S "and", (plural QP.angularVelocity), 
  S "over a period of", (phrase QP.time)]

s4_1_2_stmt3 = [S "Given the initial", (plural QP.position), S "and", 
  (plural QP.velocity), S "of a", S "set of", 
  (plural CP.rigidBody) `sC` S "determine if any of",
  S "them will collide with one another over a period of", 
  (phrase QP.time)]

s4_1_2_stmt4 = [S "Given the", (plural physicalProperty) :+: S ",", 
  S "initial linear and angular", (plural QP.position), 
  S "and", (plural QP.velocity) `sC` S "determine the new",
  (plural QP.position), S "and", (plural QP.velocity),
  S "over a period of", (phrase QP.time), S "of",
  (plural CP.rigidBody), S "that have undergone a", 
  (phrase CP.collision)]

s4_1_2_list' :: [Sentence]
s4_1_2_list' = map (foldlSent) [s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, s4_1_2_stmt4]

s4_1_2_list = enumSimple 1 (getAcc goalStmt) s4_1_2_list'

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

s4_2 :: Section

s4_2 = SRS.solCharSpec []
 [s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5, s4_2_6]

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

s4_2_1 :: Section
s4_2_1_intro, s4_2_1_list :: Contents

s4_2_1 = SRS.assump [s4_2_1_intro, s4_2_1_list] []

-- TODO: Add assumption references in the original and this SRS. --
s4_2_1_intro = foldlSP 
  [S "This", (phrase section_), S "simplifies the original", (phrase problem),
  S "and helps in developing the", (phrase thModel), S "by filling in the",
  S "missing", (phrase information), S "for the" +:+. (phrase physicalSystem),
  S "The numbers given in", S "the square brackets refer to the", 
  foldr1 sC (map (refs) itemsAndRefs) `sC` S "or", 
  refs (likelyChg, s6) `sC` S "in which the respective",
  (phrase assumption), S "is used"]
  where refs (chunk, ref) = (titleize' chunk) +:+ (sSqBr $ makeRef ref)

itemsAndRefs :: [(CI, Section)]
itemsAndRefs = [(thModel, s4_2_2), (genDefn, s4_2_3), (dataDefn, s4_2_4), 
  (inModel, s4_2_5)]

s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4, s4_2_1_assum5, 
  s4_2_1_assum6, s4_2_1_assum7 :: [Sentence]

s4_2_1_assum1 = [S "All objects are", (plural CP.rigidBody)]
s4_2_1_assum2 = [S "All objects are", (getAcc twoD)]
s4_2_1_assum3 = [S "The library uses a", (phrase CP.cartesian)]
s4_2_1_assum4 = [S "The axes are defined using", 
  (phrase CP.rightHand)]
s4_2_1_assum5 = [S "All", (plural CP.rigidBody), 
  (plural CP.collision), S "are vertex-to-edge", 
  (plural CP.collision)]
s4_2_1_assum6 = [S "There is no damping", 
  S "involved throughout the", (phrase simulation)]
s4_2_1_assum7 = [S "There are no", (plural CM.constraint),
  S "and", (plural CP.joint), S "involved throughout the", 
  (phrase simulation)]

s4_2_1_list' :: [Sentence]
s4_2_1_list' = map (foldlSent) [s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4,
               s4_2_1_assum5, s4_2_1_assum6, s4_2_1_assum7]

s4_2_1_list = enumSimple 1 (getAcc assumption) s4_2_1_list'


--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

s4_2_2 :: Section
s4_2_2_TMods :: [Contents]

s4_2_2 = thModF (chipmunk) s4_2_2_TMods

s4_2_2_TMods = map cpSymMapT cpTMods

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

s4_2_3 :: Section
s4_2_3_intro :: Contents
-- s4_2_3_GDefs :: [Contents]

s4_2_3 = SRS.genDefn ([s4_2_3_intro] {- ++
  (map Con s4_2_3_GDefs)-}) []

s4_2_3_intro = foldlSP 
  [S "This", (phrase section_), S "collects the laws and", 
  (plural CM.equation), S "that will be used in deriving the", 
  (plural dataDefn) `sC` S "which in turn will be used to build the", 
  (plural inModel)]

-- GDefs not yet implemented --
{-
s4_2_3_GDefs :: [Contents]
s4_2_3_GDefs = map (Definition . General) gDefs)
-}

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

s4_2_4 :: Section
s4_2_4_intro :: Sentence
s4_2_4_DDefs :: [Contents]

s4_2_4 = dataDefnF s4_2_4_intro s4_2_4_DDefs

s4_2_4_intro = foldlSent [S "The", (phrase CPP.dimension), 
  S "of each", (phrase quantity), S "is also given"]

s4_2_4_DDefs = map cpSymMapD cpDDefs

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5 :: Section
s4_2_5_IMods :: [Contents]

s4_2_5 = inModelF s4_1 s4_2_4 s4_2_2 s4_2_3 s4_2_5_IMods

-- Instance models not fully yet implemented --

s4_2_5_IMods = map cpSymMapT iModels

------------------------------
-- Collision Diagram        --
------------------------------
{-- should be paired with the last instance model for this example
secCollisionDiagram = Paragraph $ foldlSent [ S "This section presents an image", 
  S "of a typical collision between two 2D rigid bodies labeled A and B,"  
  S "showing the position of the two objects, the collision normal vector n and",
  S "the vectors from the approximate center of mass of each object to the point",
  S "of collision P, rAP and rBP. Note that this figure only presents", 
  S "vertex-to-edge collisions, as per our assumptions (A5)."]
--}

{--fig_1 = Figure (titleize figure +:+ S "1:" +:+ S "Collision between two rigid bodies")
"CollisionDiagram.png" --}
------------------------------
-- 4.2.6 : Data Constraints --
------------------------------

s4_2_6 :: Section
s4_2_6_table1, s4_2_6_table2 :: Contents

s4_2_6 = datConF ((makeRef s4_2_6_table1) +:+ S "and" +:+ 
  (makeRef s4_2_6_table2) +:+ S "show") EmptyS True EmptyS 
  [s4_2_6_table1, s4_2_6_table2]


-- Currently unable to write relations in sentences, so verbal explanations
-- will do for now.
-- How do I write 2pi in constraints?

lengthConstraint, massConstraint, mmntOfInConstraint, gravAccelConstraint, 
  posConstraint, veloConstraint, orientConstraint, angVeloConstraint, 
  forceConstraint, 
  torqueConstraint :: [Sentence]

lengthConstraint = makeConstraint lengthCons (S "44.2")
massConstraint = makeConstraint massCons (S "56.2")
mmntOfInConstraint = makeConstraint mmntOfInCons (S "74.5")
gravAccelConstraint = makeConstraint gravAccelCons (S "9.8")
posConstraint = makeConstraint posCons (S "(0.412, 0.502)")
veloConstraint = makeConstraint veloCons (S "2.51")
orientConstraint = makeConstraint orientCons (S "pi/2")
angVeloConstraint = makeConstraint angVeloCons (S "2.1")
forceConstraint = makeConstraint forceCons (S "98.1")
torqueConstraint = makeConstraint torqueCons (S "200")



restCoefConstraint :: [Sentence]
restCoefConstraint = makeConstraint restCoefCons (S "0.8")


s4_2_6_t1_list, s4_2_6_t2_list :: [[Sentence]]
s4_2_6_t1_list = [lengthConstraint, massConstraint, 
  mmntOfInConstraint, gravAccelConstraint, posConstraint, veloConstraint, 
  orientConstraint, angVeloConstraint, forceConstraint, torqueConstraint,
  restCoefConstraint]

s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, S "Typical Value"]
  (s4_2_6_t1_list) (S "Table 1:" +:+ (titleize input_) +:+ S "Variables") True


s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint]
  (mkTable [(\x -> x!!0), (\x -> x!!1)] s4_2_6_t2_list) 
  (S "Table 2:" +:+ (titleize output_) +:+ S "Variables") True

s4_2_6_t2_list = [posConstraint, veloConstraint, 
  orientConstraint, angVeloConstraint]

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

s5 :: Section
s5 = reqF [s5_1, s5_2]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1_list :: Contents

s5_1 = SRS.funcReq [s5_1_list] []

s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
  s5_1_req7, s5_1_req8 :: Sentence

reqFrame :: Sentence -> Sentence -> Sentence -> Sentence -> Sentence
reqFrame a b x z = foldlSent [S "Determine the", a, S "and", b, S "over a period of", 
  (phrase QP.time), S "of the", x, z]

reqS :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence -> Sentence
reqS a b d = reqFrame (plural a) (plural b) ((getAcc twoD) +:+ (plural CP.rigidBody)) d
reqS' :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence
reqS' a b = reqS a b EmptyS 

-- some requirements look like they could be parametrized
s5_1_req1 = foldlSent [S "Create a", (phrase CP.space), S "for all of the",
  (plural CP.rigidBody), S "in the", (phrase physicalSim), 
  S "to interact in"]

s5_1_req2 = foldlSent [S "Input the initial", 
  (plural QPP.mass) `sC` (plural QP.velocity) `sC` 
  (plural QM.orientation) `sC` (plural QP.angularVelocity ), 
  S "of" `sC` S "and", (plural QP.force), S "applied on", 
  (plural CP.rigidBody)]

s5_1_req3 = foldlSent [S "Input the", (phrase CM.surface), 
  (plural property), S "of the bodies, such as", (phrase CP.friction), 
  S "or", (phrase CP.elasticity)]

s5_1_req4 = foldlSent [S "Verify that the inputs", 
  S "satisfy the required", plural physicalConstraint, S "from", (makeRef s4_2_6_table1)]

s5_1_req5 = reqS (QP.position) (QP.velocity) 
  (S "acted upon by a" +:+ (phrase QP.force))

s5_1_req6 = reqS' (QM.orientation) (QP.angularVelocity )

s5_1_req7 = foldlSent [S "Determine if any of the", 
  (plural CP.rigidBody), S "in the", (phrase CP.space), 
  S "have collided"]

s5_1_req8 = reqS (QP.position) (QP.velocity) 
  (S "that have undergone a" +:+ (phrase CP.collision))



-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
s5_1_list' :: [Sentence]
s5_1_list' = [s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
  s5_1_req7, s5_1_req8]

s5_1_list = enumSimple 1 (getAcc requirement) s5_1_list'

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

s5_2 :: Section
s5_2_intro :: Contents

s5_2 = SRS.nonfuncReq [s5_2_intro] []

s5_2_intro = foldlSP 
  [(titleize' game), S "are resource intensive, so performance",
  S "is a high priority. Other", (phrase nonfunctional), plural requirement,
  S "that are a",
  S "priority are: correctness, understandability, portability,",
  S "reliability, and maintainability"]

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6_intro, s6_list :: Contents

s6 = SRS.likeChg [s6_intro, s6_list] []

s6_intro = foldlSP [S "This", (phrase section_), S "lists the", 
  (plural likelyChg), S "to be made to the", (phrase physics), (phrase game), 
  (phrase library)]

s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3, 
  s6_likelyChg_stmt4 :: Sentence

--these statements look like they could be parametrized
s6_likelyChg_stmt1 = (S "internal" +:+ (getAcc CM.ode) :+: 
  S "-solving algorithm used by the" +:+ (phrase library)) `maybeChanged` 
  (S "in the future")

s6_likelyChg_stmt2 = (phrase library) `maybeExpanded`
  (S "to deal with edge-to-edge and vertex-to-vertex" +:+ (plural CP.collision))

s6_likelyChg_stmt3 = (phrase library) `maybeExpanded` S "to include motion with damping"

s6_likelyChg_stmt4 = (phrase library) `maybeExpanded` (S "to include" +:+ 
  (plural CP.joint) +:+ S "and" +:+ (plural CM.constraint))

s6_list' :: [Sentence]
s6_list' = [s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3,
  s6_likelyChg_stmt4]

s6_list = enumSimple 1 (getAcc likelyChg) s6_list'


-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

s7 :: Section
s7_intro, s7_2dlist, s7_mid, s7_3dlist :: Contents

s7 = SRS.offShelfSol [s7_intro, s7_2dlist,
  s7_mid, s7_3dlist] []

s7_intro = Paragraph $ S "As mentioned in" +:+. ((makeRef s4_1) `sC`
  S "there already exist free" +:+ (phrase openSource) +:+ (phrase game) +:+
  (plural physLib)) +:+ S "Similar" +:+ (getAcc twoD) +:+ 
  (plural physLib) +:+ S "are:"

s7_2dlist = enumBullet [(S "Box2D: http://box2d.org/"),
  (S "Nape Physics Engine: http://napephys.com/")]

s7_mid = Paragraph $ foldl (+:+) (EmptyS) [S "Free", (phrase openSource), 
        S "3D", (phrase game), (plural physLib), S "include:"]

s7_3dlist = enumBullet [
  (S "Bullet: http://bulletphysics.org/"),
  (S "Open Dynamics Engine: http://www.ode.org/"),
  (S "Newton Game Dynamics: http://newtondynamics.com/")]

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------

s8 :: Section
s8 = traceMGF [s8_table1, s8_table2, s8_table3] s8_traces 
  [s8_table1, s8_table2, s8_table3] []
--s8 = SRS.traceyMandG [s8_intro1, s8_table1, s8_table2, s8_table3] []

s8_traces, s8_trace1, s8_trace2, s8_trace3 :: [Sentence]
s8_traces = map (foldlList) [s8_trace1, s8_trace2, s8_trace3]

s8_trace1 = [(plural goalStmt), (plural requirement), (plural inModel), 
  (plural datumConstraint) +:+. S "with each other"]

s8_trace2 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel) +:+. S "on the assumptions"]

s8_trace3 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel) +:+ S "on each other"]

-- these look like they could be generated by the sections above
s8_instaModel, s8_assump, s8_funcReq, s8_data, s8_goalstmt, s8_theoryModel, 
  s8_genDef, s8_dataDef, s8_likelyChg :: [String]

s8_instaModelRef, s8_assumpRef, s8_funcReqRef, s8_goalstmtRef, 
  s8_theoryModelRef, s8_genDefRef, s8_dataDefRef, s8_likelyChgRef, 
  s8_dataRef :: [Sentence]

s8_instaModel = ["IM1", "IM2", "IM3"]
s8_instaModelRef = map (refFromType Theory cpSymbMap) iModels

s8_theoryModel = ["T1", "T2", "T3", "T4", "T5"]
s8_theoryModelRef = map (refFromType Theory cpSymbMap) cpTMods

s8_dataDef = ["DD1","DD2","DD3","DD4","DD5","DD6","DD7","DD8"]
s8_dataDefRef = map (refFromType Data cpSymbMap) cpDDefs

s8_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7"]
s8_assumpRef = makeListRef s4_2_1_list' s4_2_1

s8_funcReq =  ["R1","R2","R3", "R4", "R5", "R6", "R7", "R8"]
s8_funcReqRef = makeListRef s5_1_list' s5_1

s8_data = ["Data Constraints"]
s8_dataRef = [makeRef s4_2_6]

s8_goalstmt = ["GS1", "GS2", "GS3", "GS4"]
s8_goalstmtRef = makeListRef s4_1_2_list' s4_1_2

s8_genDef = ["GD1", "GD2", "GD3", "GD4", "GD5", "GD6", "GD7"]
s8_genDefRef = makeListRef s8_genDef s4_2_3

s8_likelyChg = ["LC1", "LC2", "LC3", "LC4"]
s8_likelyChgRef = makeListRef s6_list' s6


{-- Matrices generation below --}

gS1_t1, gS2_t1, gS3_t1, gS4_t1, r1_t1, r2_t1, r3_t1, r4_t1, r5_t1, r6_t1, r7_t1, 
  r8_t1 :: [String]
gS1_t1 = ["IM1"]
gS2_t1 = ["IM2"]
gS3_t1 = ["IM3"]
gS4_t1 = ["IM3", "R7"]
r1_t1 = []
r2_t1 = ["IM1", "IM2", "R4"]
r3_t1 = ["IM3", "R4"]
r4_t1 = ["Data Constraints"]
r5_t1 = ["IM1"]
r6_t1 = ["IM2"]
r7_t1 = ["R1"]
r8_t1 = ["IM3", "R7"]

s8_row_header_t1, s8_col_header_t1 :: [Sentence]
s8_row_header_t1 = zipWith itemRefToSent s8_row_t1 (s8_instaModelRef ++ 
  (take 3 s8_funcReqRef) ++ s8_dataRef)
s8_col_header_t1 = zipWith itemRefToSent 
  (s8_goalstmt ++ s8_funcReq) (s8_goalstmtRef ++ s8_funcReqRef)

s8_row_t1 :: [String]
s8_row_t1 = s8_instaModel ++ ["R1","R4","R7"] ++ s8_data

s8_columns_t1 :: [[String]]
s8_columns_t1 = [gS1_t1, gS2_t1, gS3_t1, gS4_t1, r1_t1, r2_t1, r3_t1, r4_t1, 
  r5_t1, r6_t1, r7_t1, r8_t1]

s8_table1 :: Contents
s8_table1 = Table (EmptyS:(s8_row_header_t1))
  (makeTMatrix s8_col_header_t1 s8_columns_t1 s8_row_t1)
  (showingCxnBw (traceyMatrix) (titleize' requirement +:+ sParen (makeRef s5)
  `sC` (titleize' goalStmt) +:+ sParen (makeRef s4_1_2) +:+ S "and Other" +:+
  titleize' item)) True

s8_columns_t2 :: [[String]]
s8_columns_t2 = [t1_t2, t2_t2, t3_t2, t4_t2, t5_t2, gD1_t2, gD2_t2, gD3_t2,
  gD4_t2, gD5_t2, gD6_t2, gD7_t2, dD1_t2, dD2_t2, dD3_t2, dD4_t2, dD5_t2, dD6_t2,
  dD7_t2, dD8_t2, iM1_t2, iM2_t2, iM3_t2, lC1, lC2, lC3, lC4]

t1_t2, t2_t2, t3_t2, t4_t2, t5_t2, gD1_t2, gD2_t2, gD3_t2, gD4_t2, gD5_t2, 
  gD6_t2, gD7_t2, dD1_t2, dD2_t2, dD3_t2, dD4_t2, dD5_t2, dD6_t2, dD7_t2, dD8_t2, 
  iM1_t2, iM2_t2, iM3_t2, lC1, lC2, lC3, lC4 :: [String]
t1_t2 = []
t2_t2 = []
t3_t2 = []
t4_t2 = ["A1"]
t5_t2 = []
gD1_t2 = []
gD2_t2 = []
gD3_t2 = ["A2","A3"]
gD4_t2 = []
gD5_t2 = []
gD6_t2 = []
gD7_t2 = []
dD1_t2 = ["A1","A2"]
dD2_t2 = ["A1","A2","A6"]
dD3_t2 = ["A1","A2","A6"]
dD4_t2 = ["A1","A2","A6"]
dD5_t2 = ["A1","A2","A6"]
dD6_t2 = ["A1","A2","A6"]
dD7_t2 = ["A1","A2","A6"]
dD8_t2 = ["A1","A2","A4","A5"]
iM1_t2 = ["A1","A2","A6","A7"]
iM2_t2 = ["A1","A2","A4","A6","A7"]
iM3_t2 = ["A1","A2","A5","A6","A7"]
lC1 = []
lC2 = ["A5"]
lC3 = ["A6"]
lC4 = ["A7"]

s8_row_t2, s8_cols_t2 :: [String]
s8_row_t2 = s8_assump

s8_cols_t2 = (s8_theoryModel ++ s8_genDef ++ s8_dataDef ++ s8_instaModel ++
  s8_likelyChg) 
s8_cols_ref_t2 :: [Sentence]
s8_cols_ref_t2 = (s8_theoryModelRef ++ s8_genDefRef ++ s8_dataDefRef ++ 
  s8_instaModelRef ++ s8_likelyChgRef)

s8_row_header_t2, s8_col_header_t2 :: [Sentence]
s8_row_header_t2 = zipWith itemRefToSent (s8_row_t2) (s8_assumpRef)
s8_col_header_t2 = zipWith itemRefToSent (s8_cols_t2) (s8_cols_ref_t2)

s8_table2 :: Contents
s8_table2 = Table (EmptyS:s8_row_header_t2)
  (makeTMatrix s8_col_header_t2 s8_columns_t2 s8_row_t2) 
  (showingCxnBw (traceyMatrix) (titleize' assumption +:+ sParen (makeRef s4_2_1) 
  +:+ S "and Other" +:+ titleize' item)) True


s8_columns_t3 :: [[String]]
s8_columns_t3 = [t1_t3, t2_t3, t3_t3, t4_t3, t5_t3, gD1_t3, gD2_t3, gD3_t3, 
  gD4_t3, gD5_t3, gD6_t3, gD7_t3, dD1_t3, dD2_t3, dD3_t3, dD4_t3, dD5_t3, dD6_t3,
  dD7_t3, dD8_t3, iM1_t3, iM2_t3, iM3_t3]

t1_t3, t2_t3, t3_t3, t4_t3, t5_t3, gD1_t3, gD2_t3, gD3_t3, gD4_t3, gD5_t3, gD6_t3,
  gD7_t3, dD1_t3, dD2_t3, dD3_t3, dD4_t3, dD5_t3, dD6_t3, dD7_t3, dD8_t3, iM1_t3,
  iM2_t3, iM3_t3 :: [String]

t1_t3 = [] 
t2_t3 = []
t3_t3 = []
t4_t3 = []
t5_t3 = ["GD6", "GD7"]
gD1_t3 = ["T1"]
gD2_t3 = ["T2", "GD1"]
gD3_t3 = ["T1", "T3"]
gD4_t3 = []
gD5_t3 = ["GD4"]
gD6_t3 = []
gD7_t3 = []
dD1_t3 = []
dD2_t3 = []
dD3_t3 = []
dD4_t3 = []
dD5_t3 = []
dD6_t3 = []
dD7_t3 = []
dD8_t3 = ["T4", "GD1","GD4","GD5","GD7","IM3"]
iM1_t3 = ["T1", "GD3", "DD1","DD2","DD3","DD4"]
iM2_t3 = ["T5", "DD1", "DD2", "DD3", "DD4"]
iM3_t3 = ["GD1", "GD2", "GD6", "GD7", "DD1", "DD8"]

s8_row_t3 :: [String]
s8_row_ref_t3 :: [Sentence]
s8_row_t3 = s8_theoryModel ++ s8_genDef ++ s8_dataDef ++ s8_instaModel
s8_row_ref_t3 = s8_theoryModelRef ++ s8_genDefRef ++ s8_dataDefRef ++ 
  s8_instaModelRef

s8_col_header_t3, s8_row_header_t3 :: [Sentence]
s8_col_header_t3 = zipWith itemRefToSent (s8_row_t3) (s8_row_ref_t3)
s8_row_header_t3 = s8_col_header_t3

s8_table3 :: Contents
s8_table3 = Table (EmptyS:s8_row_header_t3)
  (makeTMatrix s8_col_header_t3 s8_columns_t3 s8_row_t3)
  (showingCxnBw (traceyMatrix) (titleize' item +:+ 
  S "and Other" +:+ titleize' section_)) True

----------------
-- REFERENCES --
----------------
--}
-- To be added --

s9 :: Section
s9 = SRS.reference [s9_list] []

s9_list :: Contents
s9_list = mkRefsList 1 (map (foldl (+:+) EmptyS) [s9_ref1, s9_ref2, s9_ref3, 
  s9_ref4, s9_ref5, s9_ref6, s9_ref7, s9_ref8, s9_ref9, s9_ref10])

-- make sure all refs are proper format

s9_ref1, s9_ref2, s9_ref3, s9_ref4, s9_ref5, s9_ref6, s9_ref7, 
  s9_ref8, s9_ref9, s9_ref10 :: [Sentence]

s9_ref1 = [S "David L. Parnas.", S "Designing Software for Ease of Extension",
  S "and Contraction.", S "ICSE '78: Proceedings of the 3rd international", 
  S "conference on Software engineering,", S "264-277, 1978"]

s9_ref2 = [S "Greg Wilson and D.A. Aruliah and C. Titus Brown and Neil P.", 
  S "Chue Hong and Matt Davis and Richard T. Guy and Steven H.D. Haddock", 
  S "and Kathryn D. Huff and Ian M. Mitchell and Mark D. Plumblet and Ben Waugh", 
  S "and Ethan P. White and Paul Wilson. Best Practices for Scientific", 
  S "Computing, 2013"]

s9_ref3 = [S "David L. Parnas. On the Criteria To Be Used in Decomposing Systems", 
  S "into Modules. Comm. ACM, vol. 15, no. 2, pp. 1053-1058, 1972"]

s9_ref4 = [S "D. L. Parnas and P. C. Clements and D. M. Weiss.",
  S "The Modular Structure of Complex Systems.", S "ICSE '84: Proceedings of", 
  S "the 7th international conference on Software engineering" `sC` 
  S "408-417, 1984"]

s9_ref5 = [S "David L. Parnas and P.C. Clements.", S "A Rational Design", 
  S "Process: How and Why to Fake it.", S "IEEE Transactions on Software", 
  S "Engineering,", S "251-257" `sC` S "1986"]

s9_ref6 = [S "Nirmitha Koothoor. A document drive approach to certifying" +:+. 
  (phrase sciCompS), S "Master's thesis, McMaster University,", 
  S "Hamilton, Ontario, Canada, 2013."]

s9_ref7 = [S "David L. Parnas and P.C. Clements. A rational design process: How", 
  S "and why to fake it. IEEE Transactions on Software Engineering,", 
  S "12(2):251-257, February 1986."]

s9_ref8 = [S "W. Spencer Smith and Lei Lai. A new requirements template for",
  S "scientific computing. In J. Ralyt" :+: (F Acute 'e') `sC` 
  S "P. Agerfalk, and N. Kraiem,", S "editors, Proceedings of the First", 
  S "International Workshopon", S "Situational Requirements Engineering", 
  S "Processes - Methods,", S "Techniques and Tools to Support Situation-Specific", 
  S "Requirements", S "Engineering Processes, SREP'05, pages 107-121, Paris, France,", 
  S "2005. In conjunction with 13th IEEE International Requirements", 
  S "Engineering Conference."]

s9_ref9 = [S "J. Frederick Bueche. Introduction to Physics for Scientists", 
  S "Fourth Edition. 1986"]

s9_ref10 = [S "Marilyn Lightstone. Derivation of Tank/PCM Model. 2012"]