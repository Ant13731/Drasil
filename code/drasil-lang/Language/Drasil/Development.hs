{- re-export many things to simplify external use -}
module Language.Drasil.Development (
  -- NounPhrase
    NounPhrase(phraseNP,pluralNP)
  -- Expr
  , UFuncB(..), UFuncVec(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..), OrdBinOp(..)
  -- Expr.Extract
  , dep, names, names', namesRI
  -- Sentence.Extract
  , sdep, lnames, lnames'
  -- Expr.Precendence
  , precA, precB, eprec
  -- Testing tools
  , dumpRCQD, dumpQD
  ) where

import Control.Lens

import Language.Drasil
import Language.Drasil.NounPhrase.Core
import Language.Drasil.Expr
import Language.Drasil.Expr.Extract (dep, names', names, namesRI)
import Language.Drasil.Expr.Precedence (precA, precB, eprec)
import Language.Drasil.Sentence.Extract(sdep, lnames, lnames')

-- dump common things between a RC and a QD
dumpRCQD :: (HasUID a,
             NamedIdea a,
             Idea a,
             Definition a,
             ConceptDomain a,
             ExprRelat a) 
            => 
            a -> IO ()
dumpRCQD trg = do
  putStrLn "uid:"
  putStrLn (trg ^. uid)

  putStrLn "term:"
  case (trg ^. term) of
    (ProperNoun s _) -> putStrLn s
    (CommonNoun s _ _) -> putStrLn s
    (Phrase _ _ _ _) -> putStrLn "PHRASE PRINT NOT IMPLEMENTED"
  
  putStrLn "getA:"
  case getA trg of
    Just s -> putStrLn s
    Nothing -> putStrLn "No Idea abbreviation"

  putStrLn "defn:"
  -- TODO: convert `defn trg` into a String?

  putStrLn "cdom:"
  print $ cdom trg

  putStrLn "relat:"
  -- TODO: convert `relat trg` into a String?

-- QDefinition has a few more things than a  RelationConcept
dumpQD :: QDefinition -> IO ()
dumpQD trg = do
  dumpRCQD trg

  putStrLn "typ:"
  print $ trg ^. typ

  putStrLn "symbol:"
  -- TODO: convert `symbol trg` into a String?

  putStrLn "defnExpr"
  -- TODO: convert `trg ^. defnExpr` into a String?

  putStrLn "getUnit"
  -- TODO: convert `getUnit trg` into a String?

