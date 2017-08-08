{-# LANGUAGE GADTs #-}
-- | Contains Sentences and helpers
module Language.Drasil.Spec where

import Language.Drasil.Unicode (Greek,Special,Special(CurlyBrOpen,CurlyBrClose,SqBrOpen,SqBrClose))
import Language.Drasil.Symbol
import Language.Drasil.Expr

-- | For writing accented characters
data Accent = Grave | Acute deriving Eq

-- | For writing "sentences" via combining smaller elements
-- Sentences are made up of some known vocabulary of things:
-- - units (their visual representation)
-- - words (via String)
-- - special characters
-- - accented letters
-- - References to specific layout objects
infixr 5 :+:
data Sentence where
  Sy    :: USymb -> Sentence
  S     :: String -> Sentence       -- Strings, used for Descriptions in Chunks
  G     :: Greek -> Sentence
  Sp    :: Special -> Sentence
  P     :: Symbol -> Sentence
  F     :: Accent -> Char -> Sentence  -- Special formatting for certain special
                                       -- chars
  Ref   :: RefType -> Sentence -> Sentence  -- Needs helper func to create Ref
                                    -- See Reference.hs
  Quote :: Sentence -> Sentence     -- Adds quotation marks around a sentence
                                    
  -- Direct concatenation of two Specs (no implicit spaces!)
  (:+:) :: Sentence -> Sentence -> Sentence   
  EmptyS :: Sentence
  E :: Expr -> Sentence

--Moving this here to avoid cyclic imports
-- | Language of unit equations, to define a unit relative
-- to another
data USymb = UName Symbol
           | UProd [USymb] -- ^ Product
           | UPow USymb Integer -- ^ can be negative, should not be 0
           | UDiv USymb USymb   -- ^ Get proper division (not neg pow)
                                -- necessary for things like J/(kg*C)

-- | For building references. Defines the possible type of reference.
data RefType = Tab -- ^ Table
             | Fig -- ^ Figure
             | Sect -- ^ Section
             | Def (Maybe Sentence)-- ^ Definition (includes theoretical models)
             | Mod -- ^ Module
             | Req (Maybe Sentence)-- ^ Requirement
             | Assump (Maybe Sentence)-- ^ Assumption
             | LC (Maybe Sentence)-- ^ Likely Change
             | UC -- ^ Unlikely Change

instance Show RefType where
  show Tab = "Table"
  show Fig = "Figure"
  show Sect = "Section"
  show Mod = "Module"
  -- show (Req (S r)) = r
  -- show (Assump (S a)) = a
  -- show (LC (S lc)) = lc
  show (Def Nothing) = "Definition"
  show (Req Nothing) = "Requirement"
  show (Assump Nothing) = "Assumption"
  show (LC Nothing) = "Likely Change"
  show UC = "Unlikely Change"
  show _ = error "Type not recognized"

-- | Helper function for wrapping sentences in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

sParenNum :: Int -> Sentence
sParenNum y = sParen (S (show y))

-- | Helper function for wrapping sentences in square brackets.
sSqBr :: Sentence -> Sentence
sSqBr x = Sp SqBrOpen :+: x :+: Sp SqBrClose

sSqBrNum :: Int -> Sentence
sSqBrNum y = sSqBr (S (show y))

sCurlyBr :: Sentence -> Sentence
sCurlyBr x = Sp CurlyBrOpen :+: x :+: Sp CurlyBrClose

-- | Helper for concatenating two sentences with a space between them.
(+:+) :: Sentence -> Sentence -> Sentence
EmptyS +:+ b = b
a +:+ EmptyS = a
a +:+ b = a :+: S " " :+: b

-- | Helper for concatenating two sentences with a comma and space between them.
sC :: Sentence -> Sentence -> Sentence
a `sC` b = a :+: S "," +:+ b

-- | Helper which concatenates two sentences using '+:+' then adds a period to
-- the end.
(+:+.) :: Sentence -> Sentence -> Sentence
a +:+. b = a +:+ b :+: S "."

-- | Helper which concatenates two sentences using ':+:' then adds a period to
-- the end.
(+.) :: Sentence -> Sentence -> Sentence
a +. b = a :+: b :+: S "."

-- | Helper which concatenates two sentences using '+:+' then adds a colon to
-- the end.
(+:) :: Sentence -> Sentence -> Sentence
a +: b = a +:+ b :+: S ":"

-- | Helper for concatenating two sentences with a semi-colon and space between them.
semiCol :: Sentence -> Sentence -> Sentence
a `semiCol` b = a :+: S ";" +:+ b

sParenDash :: Sentence -> Sentence
sParenDash = \x -> S " (" :+: x :+: S ") - "

sDash :: Sentence -> Sentence -> Sentence
y `sDash` z = y +:+ S "-" +:+ z

-- Function used to derive the units of an equation based on 
-- inferUnit :: Relation -> [USymb] -> [USymb] -> USymb
-- inferUnit (:= _ _) num den = eliminate (num, den)
-- inferUnit (:+ a _) num den = inferUnit a num den
-- inferUnit (:- a _) num den = inferUnit a num den
-- inferUnit (:* a b) num den = inferUnit a (num:(analyze_n b)) den
-- inferUnit (:/ a b) num den = inferUnit a num (den:(analyze_d b))
-- inferUnit (C a) num den = eliminate ((num:(getUnit a)), den)
-- inferUnit (Deriv _ a b) num den = eliminate ((num:(analyze_n a)), (den:(analyze_d b)))
  -- where analyze_n (Deriv _ (C a) _) = getUnit a
        -- analyze_n (C a) = getUnit a
        -- analyze_d (Deriv _ _ (C b)) = getUnit b
        -- analyze_d (C b) = getUnit b
        
-- eliminate :: ([USymb], [USymb]) -> USymb
-- eliminate ((frstnum:rstnum), den) = match frstnum den
  -- where match unt [] = unt
          
        -- match unt (frst:rst)
          -- | unt == frst
          -- | otherwise = match unt rst

instance Eq USymb where
  a == b = a == b