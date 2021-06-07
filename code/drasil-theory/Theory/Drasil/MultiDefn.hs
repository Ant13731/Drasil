{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}

module Theory.Drasil.MultiDefn (MultiDefn, DefiningExpr, 
    mkMultiDefn, mkMultiDefnForQuant, mkDefiningExpr, multiDefnGenQD) where

import Control.Lens ((^.), view, makeLenses)
import Data.List (union)
import qualified Data.List.NonEmpty as NE

import Language.Drasil hiding (DefiningExpr)

-- | A 'DefiningExpr' contains the "Expr"-related components of a QDefinition.
data DefiningExpr = DefiningExpr {
  _deUid  :: UID,      -- ^ UID
  _cd     :: [UID],    -- ^ Concept domain
  _rvDesc :: Sentence, -- ^ Defining description/statement
  _expr   :: Expr      -- ^ Defining expression
}
makeLenses ''DefiningExpr

instance HasUID        DefiningExpr where uid  = deUid
instance ConceptDomain DefiningExpr where cdom = (^. cd)
instance Definition    DefiningExpr where defn = rvDesc

-- | 'MultiDefn's are QDefinition factories, used for showing one or more ways we
--   can define a QDefinition
data MultiDefn = MultiDefn {
    _rUid  :: UID,                     -- ^ UID
    _qd    :: QuantityDict,            -- ^ Underlying quantity it defines
    _rDesc :: Sentence,                -- ^ Defining description/statement
    _rvs   :: NE.NonEmpty DefiningExpr -- ^ All possible/omitted ways we can define the related quantity
}
makeLenses ''MultiDefn


instance HasUID        MultiDefn where uid      = rUid
instance HasSymbol     MultiDefn where symbol   = symbol . (^. qd)
instance NamedIdea     MultiDefn where term     = qd . term
instance Idea          MultiDefn where getA     = getA . (^. qd)
instance HasSpace      MultiDefn where typ      = qd . typ
instance Quantity      MultiDefn where
instance MayHaveUnit   MultiDefn where getUnit  = getUnit . view qd
-- | The concept domain of a MultiDefn is the union of the concept domains of the underlying variants.
instance ConceptDomain MultiDefn where cdom     = foldr1 union . NE.toList . NE.map (^. cd) . (^. rvs)
instance Definition    MultiDefn where defn     = rDesc
-- | The related Relation of a MultiDefn is defined as the quantity and the related expressions being equal
--   e.g., `q $= a $= b $= ... $= z`
instance ExprRelat     MultiDefn where relat q  = sy q $= foldr1 ($=) (NE.map (^. expr) (q ^. rvs))

-- | Smart constructor for MultiDefns, does nothing special at the moment
mkMultiDefn :: UID -> QuantityDict -> Sentence -> NE.NonEmpty DefiningExpr -> MultiDefn
mkMultiDefn = MultiDefn

-- | Smart constructor for MultiDefns defining UIDs using that of the QuantityDict
mkMultiDefnForQuant :: QuantityDict -> Sentence -> NE.NonEmpty DefiningExpr -> MultiDefn
mkMultiDefnForQuant q = mkMultiDefn (q ^. uid) q

-- | Smart constructor for DefiningExprs
mkDefiningExpr :: UID -> [UID] -> Sentence -> Expr -> DefiningExpr
mkDefiningExpr = DefiningExpr

-- | Converting MultiDefns into QDefinitions via choosing a DefiningExpr
multiDefnGenQD :: MultiDefn -> UID -> QDefinition
multiDefnGenQD md u | length matches == 1 = datadef $ getUnit md
                    | otherwise           = error $ "Invalid UID for multiDefn QD generation; " ++ u
  where matches = NE.filter (\x -> x ^. uid == u) (md ^. rvs)
        matched = head matches
        datadef (Just a) = fromEqnSt  (md ^. qd . uid) (md ^. term) (md ^. defn) (symbol md) (md ^. typ) a (matched ^. expr)
        datadef Nothing  = fromEqnSt' (md ^. qd . uid) (md ^. term) (md ^. defn) (symbol md) (md ^. typ) (matched ^. expr)
-- TODO: Clean this `datadef` function and push it back into QDefinition code area
