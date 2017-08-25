{- re-export many things to simplify external use -}
module Language.Drasil (
  -- Output.Formats
    DocType(SRS,MG,MIS,LPM,Website)
  -- SystemInformation
  , SystemInformation(..), Block(..)
  -- Recipe
  , Recipe(..)
  -- Expr
  , Expr(..), Relation, UFunc(..), BiFunc(..), Bound(..), DerivType(..), Set, Quantifier(..)
  , log, abs, sin, cos, tan, sec, csc, cot, exp, sqrt, square, euclidean, SymbolMap, symbolMap, vars
  , summation, product, cross, m2x2, vec2D, dgnl2x2
  -- all the stuff from Unicode
  , Greek(..), Special(..)
  -- Unit
  , Unit(..), UDefn(..), DerUChunk(..), FundUnit(..), UnitDefn(..)
  , from_udefn , makeDerU, unitCon
  , (^:), (/:), (*:), new_unit
  -- Chunk
  , Chunk(..), VarChunk(..), ConceptChunk
  , makeVC, vc, makeVCObj, SymbolForm
  , dcc, dccWDS, dccWDS', cv, dcc', vc', vc'', ccs, cc, cc'
  , Quantity(..), QWrapper, qw, ConVar(..), cvR, cvRs
  , Concept(..), cw, CWrapper
  , CommonIdea(..)
  --, commonidea, CI
  , commonIdea, CI, commonIdea', commonIdea''
  -- Chunk.NamedIdea
  , NamedIdea(..), NamedChunk, short, nc, nc', npnc
  , compoundterm, for, for', for'', of_, of_', of_'', of__, of'', compoundNC, compoundNC'
  , compoundNC'', compoundNC''', npnc', with, with', and_, and_', andRT, aNP, the, a_
  , ofA,theCustom, this
  -- Chunk.Constrained
  , Constrained(..), ConstrainedChunk(..), Constraint(..), ConstrConcept(..)
  , physc, sfwrc, constrained, cuc, cvc, constrained', cuc', constrainedNRV'
  , ConstrWrapper(..), cnstrw
  , createCnstrnts
  -- Chunk.Eq
  , QDefinition(..), fromEqn, fromEqn', fromEqn'', getVC, equat
  -- Chunk.UncertainQuantity
  , UncertainQuantity(..), UncertainChunk(..), UncertQ, uq, uqNU, uqc, uqcNU, uqcND, uncrtnChunk, uvc
  , UncertainWrapper(..), uncrtnw
  -- Chunk.Unital
  , UnitalChunk(..), makeUCWDS, ucFromCV
  , uc, uc', ucs, ucs', ucsWS
  -- Chunk.Unitary
  , Unitary(..), UnitaryChunk, unitary
  -- Chunk.Relation
  , NamedRelation, makeNR, RelationConcept, makeRC, makeRC', relat, ExprRelat
  -- Chunk.Method
  , MethodChunk, fromEC, makeStdInputMethod, makeFileInputMethod
  , makeFileOutputMethod, makeMainMethod, input, output, exc, methcc, mType
  -- Chunk.Module
  , ModuleChunk, makeRecord, makeImpModule, makeImpModuleNoGen, makeUnimpModule
  , imp, hier, field, formatName, method, secret, uses
  -- Chunk.Req
  , ReqChunk(..), emptyN
  -- Chunk.LC
  , LCChunk(..)
  -- Chunk.Other
  , AssumpChunk, UCChunk
  --Chunk.Wrapper
  , cqs, qs, nw, CQSWrapper, QSWrapper, NWrapper
  --Chunk.UWrapper 
  , UWrapper, uw, ucw, UCWrapper
  --Citations
  , BibRef, City, State, Citation(..), CiteField(..), Month(..), getAuthors, getYear
  -- Spec
  , USymb(..), Sentence(..), Accent(..), sParen, sParenNum, sSqBr, sSqBrNum
  , (+:+), (+:+.), (+.), sC, (+:), semiCol, sParenDash
  , sDash
  -- NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase', at_start, at_start'
  , CapitalizationRule(..)
  , PluralRule(..), compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', titleize, titleize'
  , nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Document
  , LayoutObj(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListPair
  , section
  -- Reference
  , makeRef, acroTest
  -- Space
  , Space(..)
  -- Symbol
  , Symbol(..), sub, sup, vec, hat, prime, sCurlyBrSymb
  -- SymbolAlphabet
  , cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ
  , lA, lB, lC, lD, lE, lF, lG, lH, lI, lJ, lK, lL, lM, lN, lO, lP, lQ, lR, lS, lT, lU, lV, lW, lX, lY, lZ
  -- Misc
  , mkTable, unit'2Contents, getAcc, unit_symb, introduceAbb, phrase, plural, phrase's, plural's
  -- Printing.Helpers
  , capitalize, paren, sqbrac
  -- Generate
  , gen, genCode
  -- People
  , People, Person, person, HasName, name, manyNames, person', personWM
  , personWM', mononym
  -- CodeSpec
  , CodeSpec, codeSpec, codeSpec', Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Lang(..), Comments(..)
  , defaultChoices
  , Mod(..), packmod, FuncDef(..), FuncStmt(..), funcDef, fasg, ffor, fdec -- hacks
  , relToQD, funcData, funcQD, Func(..), asExpr, asVC   -- hacks
  -- DataDesc
  , DataDesc
  , entry, listEntry, junk, singleton, junkLine, singleLine, multiLine
  , multiLine', straight, repeated, repeated', Ind(..)
  -- Chunk.Theory
  , Theory(..), tc', TheoryChunk, TheoryModel, tm, tw
  -- Chunk.SymbolForm
  , SF, SymbolChunk, sc
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)
import Language.Drasil.SystemInformation
import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), BiFunc(..), 
          Bound(..),DerivType(..), Set, Quantifier(..))
import Language.Drasil.Expr.Math (log, sin, cos, tan, sqrt, square, sec, csc, cot, exp,
          summation, product, cross, m2x2, vec2D, dgnl2x2, euclidean)
import Language.Drasil.Expr.Extract (vars)
import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,LPM,Website))
import Language.Drasil.Document (LayoutObj(..), Document(..), DType(..)
  , Section(..), Contents(..), SecCons(..), ListType(..), ItemType(..), section
  , ListPair)
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Unicode -- all of it
import Language.Drasil.Unit -- all of it
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm hiding (symbol)
import Language.Drasil.Chunk.CommonIdea
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.UncertainQuantity
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.Chunk.Eq (QDefinition(..), fromEqn, fromEqn', fromEqn'', getVC, equat)
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Theory
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUCWDS, ucFromCV
                                  , uc, uc', ucs, ucs', ucsWS)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.Relation(NamedRelation, makeNR, RelationConcept, 
                                      makeRC, makeRC')
import Language.Drasil.Chunk.Req
import Language.Drasil.Chunk.LC
import Language.Drasil.Chunk.Method
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Other
import Language.Drasil.Chunk.Wrapper
import Language.Drasil.Chunk.Wrapper.QSWrapper
import Language.Drasil.Chunk.Wrapper.UWrapper
import Language.Drasil.ChunkDB (SymbolMap, symbolMap)
import Language.Drasil.Citations
import Language.Drasil.NounPhrase hiding (at_start, at_start', titleize
                                          , titleize', phrase, plural)
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec (USymb(..), Sentence(..), Accent(..), 
                              sParen, sParenNum, sSqBr, sSqBrNum, sC, (+:+), (+:+.), (+.), (+:),
                              semiCol, sParenDash, sDash)
import Language.Drasil.Reference (makeRef, acroTest)
import Language.Drasil.Symbol (Symbol(..), sub, sup, vec, hat, prime, sCurlyBrSymb)
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.Printing.Helpers (capitalize, paren, sqbrac)
import Language.Drasil.Generate
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
                               ,person', personWM, personWM', mononym, name)
import Language.Drasil.CodeSpec
import Language.Drasil.DataDesc