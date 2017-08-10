{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeEntity(..), CodeName(..), CodeChunk(..), CodeDefinition(..),
    codevar, qtoc, codeEquat, 
    ConstraintMap, constraintMap, physLookup, sfwrLookup, constraintLookup,
    symbToCodeName, CodeType(..),
    spaceToCodeType
  ) where

import Control.Lens

import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk

import Language.Drasil.Space as S
import Language.Drasil.Code.Code as G (CodeType(..))

import Language.Drasil.Expr
import Language.Drasil.Unicode
import Language.Drasil.Spec
import Language.Drasil.Symbol
import Language.Drasil.NounPhrase

import Data.String.Utils (replace)
import qualified Data.Map as Map
import Prelude hiding (id)

-- not using lenses for now
class (Chunk c) => CodeIdea c where
  codeName      :: c -> String

class (CodeIdea c, Quantity c) => CodeEntity c where
  codeType      :: c -> CodeType

data CodeName where
  SFCN :: (SymbolForm c) => c -> CodeName
  NICN :: (NamedIdea c)  => c -> CodeName
  
instance Chunk CodeName where
  id = cnlens id
instance CodeIdea CodeName where
  -- want to take symbol lens from SymbolForm and apply symbToCodeName to it
  -- to make codeName lens for CodeName
  codeName (SFCN c) = symbToCodeName (c ^. symbol)
  -- want to take term lens from NamedIdea and apply sentenceToCodeName to it
  -- to make codeName lens for CodeName
  codeName (NICN c) = sentenceToCodeName (phrase $ c ^. term)
instance Eq CodeName where
  c1 == c2 = 
    (c1 ^. id) == (c2 ^. id)

cnlens :: (forall c. (Chunk c) => Simple Lens c a) 
           -> Simple Lens CodeName a
cnlens l f (SFCN a) = fmap (\x -> SFCN (set l x a)) (f (a ^. l))
cnlens l f (NICN a) = fmap (\x -> NICN (set l x a)) (f (a ^. l))
--sfcnlens :: (forall c. (SymbolForm c) => Simple Lens c a) 
--             -> Simple Lens CodeName a
--sfcnlens l f (SFCN a) = fmap (\x -> SFCN (set l x a)) (f (a ^. l))

--nicnlens :: (forall c. (NamedIdea c) => Simple Lens c a) 
--             -> Simple Lens CodeName a
--nicnlens l f (NICN a) = fmap (\x -> NICN (set l x a)) (f (a ^. l))
  

sentenceToCodeName :: Sentence -> String
sentenceToCodeName (S s) = toCodeName s
sentenceToCodeName _ = error "fix"

symbToCodeName :: Symbol -> String
symbToCodeName (Atomic s) = toCodeName s
symbToCodeName (Special sp) = specialToCodeName sp
symbToCodeName (Greek g) = greekToCodeName g
symbToCodeName (Atop d sy) = decorate (symbToCodeName sy) d
symbToCodeName (Corners ul ll ur lr b) = 
  (cleft ul) ++ (cleft ll) ++ (symbToCodeName b)
    ++ (cright lr) ++ (cright ur)
  where cleft :: [Symbol] -> String
        cleft [] = ""
        cleft (s:syms) = symbToCodeName s ++ "_" ++ cleft syms
        cright :: [Symbol] -> String
        cright [] = ""
        cright (s:syms) = "_" ++ symbToCodeName s ++ cright syms
symbToCodeName (Concat sy) = concatMap symbToCodeName sy

decorate :: String -> Decoration -> String
decorate s Hat = s ++ "_hat"
decorate s Vector = s ++ "_vect"
decorate s Prime = s ++ "'"

greekToCodeName :: Greek -> String
greekToCodeName Alpha_L   = "alpha"
greekToCodeName Alpha     = "Alpha"
greekToCodeName Beta_L    = "beta"
greekToCodeName Beta      = "Beta"
greekToCodeName Chi_L     = "chi"
greekToCodeName Chi       = "Chi"
greekToCodeName Delta_L   = "delta"
greekToCodeName Delta     = "Delta"
greekToCodeName Ell       = "ell"
greekToCodeName Epsilon_L = "epsilon"
greekToCodeName Epsilon_V = "varepsilon"
greekToCodeName Epsilon   = "Epsilon"
greekToCodeName Eta_L     = "eta"
greekToCodeName Eta       = "Eta"
greekToCodeName Gamma_L   = "gamma"
greekToCodeName Gamma     = "Gamma"
greekToCodeName Iota_L    = "iota"
greekToCodeName Iota      = "Iota"
greekToCodeName Kappa_L   = "kappa"
greekToCodeName Kappa     = "Kappa"
greekToCodeName Lambda_L  = "lambda"
greekToCodeName Lambda    = "Lambda"
greekToCodeName Mu_L      = "mu"
greekToCodeName Mu        = "Mu"
greekToCodeName Nabla     = "nabla"
greekToCodeName Nu_L      = "nu"
greekToCodeName Nu        = "Nu"
greekToCodeName Omega_L   = "omega"
greekToCodeName Omega     = "Omega"
greekToCodeName Omicron_L = "omicron"
greekToCodeName Omicron   = "Omicron"
greekToCodeName Pi_L      = "pi"
greekToCodeName Pi        = "Pi"
greekToCodeName Phi_L     = "phi"
greekToCodeName Phi_V     = "varphi"
greekToCodeName Phi       = "Phi"
greekToCodeName Psi_L     = "psi"
greekToCodeName Psi       = "Psi"
greekToCodeName Rho_L     = "rho"
greekToCodeName Rho       = "Rho"
greekToCodeName Sigma_L   = "sigma"
greekToCodeName Sigma     = "Sigma"
greekToCodeName Tau_L     = "tau"
greekToCodeName Tau       = "Tau"
greekToCodeName Theta_L   = "theta"
greekToCodeName Theta     = "Theta"
greekToCodeName Upsilon_L = "upsilon"
greekToCodeName Upsilon   = "Upsilon"
greekToCodeName Xi_L      = "xi"
greekToCodeName Xi        = "Xi"
greekToCodeName Zeta_L    = "zeta"
greekToCodeName Zeta      = "Zeta"

specialToCodeName :: Special -> String
specialToCodeName Circle        = "circ"
specialToCodeName LEQ           = "leq"
specialToCodeName Partial       = "partial"
specialToCodeName UScore        = "_"
specialToCodeName Percent       = "%"
specialToCodeName CurlyBrOpen   = "{"
specialToCodeName CurlyBrClose  = "}"
specialToCodeName SqBrOpen      = "["
specialToCodeName SqBrClose     = "]"
specialToCodeName Hash          = "#" -- TODO: Double check that this is valid for 
                                      -- all of the output langs.

toCodeName :: String -> String
toCodeName s =
    let illegalChars = [
            ",","~","`","-","=","!","@","#","$","%","^","&","*","(",")","+",
            "[","]","\\",";","'",".","/","|",":","\"","<",">","?"," "]
    in foldl varNameReplace s illegalChars
    where  varNameReplace :: String -> String -> String
           varNameReplace l old = replace old "_" l


data CodeChunk where
  CodeChunk :: (Quantity c, SymbolForm c) => c -> CodeChunk
  
instance Chunk CodeChunk where
  id = qslens id
instance NamedIdea CodeChunk where
  term = qslens term
  getA (CodeChunk n) = getA n
instance SymbolForm CodeChunk where
  symbol = qslens symbol
instance Quantity CodeChunk where
  typ = qslens typ
  getSymb (CodeChunk c) = getSymb c
  getUnit (CodeChunk c) = getUnit c
instance CodeIdea CodeChunk where
  codeName (CodeChunk c) = symbToCodeName (c ^. symbol)
instance CodeEntity CodeChunk where
  codeType (CodeChunk c) = spaceToCodeType (c ^. typ)
instance Eq CodeChunk where
  (CodeChunk c1) == (CodeChunk c2) = 
    (c1 ^. id) == (c2 ^. id)

qslens :: (forall c. (Quantity c, SymbolForm c) => Simple Lens c a) 
           -> Simple Lens CodeChunk a
qslens l f (CodeChunk a) = 
  fmap (\x -> CodeChunk (set l x a)) (f (a ^. l))
  
  
codevar :: (Quantity c, SymbolForm c) => c -> CodeChunk
codevar = CodeChunk
  
  
           
data CodeDefinition where
  CodeDefinition :: (CodeEntity c, SymbolForm c) => c -> Expr -> CodeDefinition
  
instance Chunk CodeDefinition where
  id = qscdlens id
instance NamedIdea CodeDefinition where
  term = qscdlens term
  getA (CodeDefinition n _) = getA n
instance SymbolForm CodeDefinition where
  symbol = qscdlens symbol
instance Quantity CodeDefinition where
  typ = qscdlens typ
  getSymb (CodeDefinition c _) = getSymb c
  getUnit (CodeDefinition c _) = getUnit c
instance CodeIdea CodeDefinition where
  codeName (CodeDefinition c _) = codeName c
instance CodeEntity CodeDefinition where
  codeType (CodeDefinition c _) = codeType c
instance Eq CodeDefinition where
  (CodeDefinition c1 _) == (CodeDefinition c2 _) = 
    (c1 ^. id) == (c2 ^. id)

qscdlens :: (forall c. (Quantity c, SymbolForm c) => Simple Lens c a) 
            -> Simple Lens CodeDefinition a
qscdlens l f (CodeDefinition a b) = 
  fmap (\x -> CodeDefinition (set l x a) b) (f (a ^. l)) 
  
qtoc :: QDefinition -> CodeDefinition
qtoc (EC q e) = CodeDefinition (codevar q) e

codeEquat :: CodeDefinition -> Expr
codeEquat (CodeDefinition _ e) = e 



spaceToCodeType :: Space -> CodeType
spaceToCodeType S.Integer = G.Integer
spaceToCodeType S.Natural = G.Integer
spaceToCodeType S.Radians = G.Float
spaceToCodeType S.Real = G.Float
spaceToCodeType S.Rational = G.Float
spaceToCodeType S.Boolean = G.Boolean
spaceToCodeType S.Char = G.Char
spaceToCodeType S.String = G.String
spaceToCodeType (S.Vect s) = G.List (spaceToCodeType s)
spaceToCodeType (S.Obj n) = G.Object (toCodeName n)
--spaceToCodeType (S.Discrete d) = G.List (spaceToCodeType d)
--spaceToCodeType (S.DiscreteI _) = G.List (spaceToCodeType S.Integer)
--spaceToCodeType (S.DiscreteD _) = G.List (spaceToCodeType S.Rational)
--spaceToCodeType (S.DiscreteS _) = G.List (spaceToCodeType S.String)


-- codeExpr :: Expr -> Expr
-- codeExpr (a :/ b)     = (codeExpr a) :/ (codeExpr b)
-- codeExpr (a :* b)     = (codeExpr a) :* (codeExpr b)
-- codeExpr (a :+ b)     = (codeExpr a) :+ (codeExpr b)
-- codeExpr (a :^ b)     = (codeExpr a) :^ (codeExpr b)
-- codeExpr (a :- b)     = (codeExpr a) :- (codeExpr b)
-- codeExpr (a :. b)     = (codeExpr a) :. (codeExpr b)
-- codeExpr (a :&& b)    = (codeExpr a) :&& (codeExpr b)
-- codeExpr (a :|| b)    = (codeExpr a) :|| (codeExpr b)
-- codeExpr (Deriv a b c) = Deriv (codeExpr a) (codeExpr b) (codeExpr c)
-- codeExpr (Not e)      = Not (codeExpr e)
-- codeExpr (Neg e)      = Neg (codeExpr e)
-- codeExpr (C c)        = C (SFCN c)
-- codeExpr (FCall f x)  = FCall (codeExpr f) (map codeExpr x)
-- codeExpr (Case ls)    = Case (map (\(x,y) -> (codeExpr x, codeExpr y)) ls)
-- codeExpr (a := b)     = (codeExpr a) := (codeExpr b)
-- codeExpr (a :!= b)    = (codeExpr a) :!= (codeExpr b)
-- codeExpr (a :> b)     = (codeExpr a) :> (codeExpr b)
-- codeExpr (a :< b)     = (codeExpr a) :< (codeExpr b)
-- codeExpr (a :<= b)    = (codeExpr a) :<= (codeExpr b)
-- codeExpr (a :>= b)    = (codeExpr a) :>= (codeExpr b)
-- codeExpr (UnaryOp u)  = codeExpr (unpack u) m
-- codeExpr (Grouping e) = Grouping (codeExpr e)
-- codeExpr (BinaryOp b) = nub (concat $ map (\x -> codeExpr x m) (binop b))


type ConstraintMap = Map.Map String [Constraint]

constraintMap :: (Constrained c) => [c] -> ConstraintMap
constraintMap cs = Map.fromList (map (\x -> ((x ^. id), (x ^. constraints))) cs)

getPhys :: [Constraint] -> [(Expr -> Relation)]
getPhys []            = []
getPhys ((Phys c):cs) = [c] ++ getPhys cs
getPhys (_:cs)        = getPhys cs

getSfwr :: [Constraint] -> [(Expr -> Relation)]
getSfwr []            = []
getSfwr ((Sfwr c):cs) = [c] ++ getSfwr cs
getSfwr (_:cs)        = getSfwr cs

getConstraint :: Constraint -> (Expr -> Relation)
getConstraint (Sfwr c) = c
getConstraint (Phys c) = c

physLookup :: (Quantity q) => q -> ConstraintMap -> [Expr]
physLookup q m = constraintLookup' q m getPhys

sfwrLookup :: (Quantity q) => q -> ConstraintMap -> [Expr]
sfwrLookup q m = constraintLookup' q m getSfwr

constraintLookup :: (Quantity q) => q -> ConstraintMap -> [Expr]
constraintLookup q m = constraintLookup' q m (map getConstraint)

constraintLookup' :: (Quantity q) => q -> ConstraintMap 
                      -> ([Constraint] -> [(Expr -> Relation)]) -> [Expr]
constraintLookup' q m f = lookC (Map.lookup (q ^. id) m) (getSymb q)
  where lookC :: Maybe [Constraint] -> Maybe SF -> [Expr]
        lookC _ Nothing = error "constrained quantities must have symbol"
        lookC (Just cs) (Just s) = map (\x -> x (C s)) (f cs)
        lookC Nothing _ = []