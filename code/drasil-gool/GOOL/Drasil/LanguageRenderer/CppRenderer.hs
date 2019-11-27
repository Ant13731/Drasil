{-# LANGUAGE TypeFamilies, Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ code is contained in this module
module GOOL.Drasil.LanguageRenderer.CppRenderer (
  -- * C++ Code Configuration -- defines syntax of all C++ code
  CppSrcCode(..), CppHdrCode(..), CppCode(..), unCPPC
) where

import Utils.Drasil (blank, indent, indentList)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym(..), 
  InternalFile(..), KeywordSym(..), PermanenceSym(..), InternalPerm(..), 
  BodySym(..), BlockSym(..), InternalBlock(..), ControlBlockSym(..), 
  TypeSym(..), InternalType(..), UnaryOpSym(..), BinaryOpSym(..), 
  VariableSym(..), InternalVariable(..), ValueSym(..), NumericExpression(..),
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  MethodSym(..), InternalMethod(..), StateVarSym(..), InternalStateVar(..), 
  ClassSym(..), InternalClass(..), ModuleSym(..), InternalMod(..), 
  BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (addExt, enumElementsDocD, multiStateDocD, 
  bodyDocD, oneLinerD, outDoc, intTypeDocD, charTypeDocD, stringTypeDocD, 
  typeDocD, enumTypeDocD, listTypeDocD, listInnerTypeD, voidDocD, paramDocD, 
  paramListDocD, mkParam, stateVarDocD, constVarDocD, runStrategyD, listSliceD, 
  notifyObserversD, freeDocD, mkSt, mkStNoEnd, stringListVals', 
  stringListLists', stateD, loopStateD, emptyStateD, assignD, 
  assignToListIndexD, multiAssignError, decrementD, incrementD, decrement1D, 
  increment1D, constDecDefD, discardInputD, discardFileInputD, closeFileD, 
  breakD, continueD, returnD, multiReturnError, valStateD, throwD, initStateD, 
  changeStateD, initObserverListD, addObserverD, ifNoElseD, switchD, 
  switchAsIfD, forRangeD, tryCatchD, unOpPrec, notOpDocD, negateOpDocD, 
  sqrtOpDocD, absOpDocD, expOpDocD, sinOpDocD, cosOpDocD, tanOpDocD, asinOpDocD,
  acosOpDocD, atanOpDocD, unExpr, unExpr', typeUnExpr, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', 
  typeBinExpr, mkVal, mkVar, litTrueD, litFalseD, litCharD, litFloatD, litIntD, 
  litStringD, classVarCheckStatic, inlineIfD, varD, staticVarD, selfD, enumVarD,
  objVarD, listVarD, listOfD, valueOfD, argD, argsListD, objAccessD, 
  objMethodCallD, objMethodCallNoParamsD, selfAccessD, listIndexExistsD, 
  funcAppD, newObjD, newObjDocD', castDocD, castObjDocD, funcD, getD, setD, 
  listSizeD, listAddD, listAppendD, iterBeginD, iterEndD, listAccessD, listSetD,
  getFuncD, setFuncD, listSizeFuncD, listAppendFuncD, listAccessFuncD', 
  listSetFuncD, staticDocD, dynamicDocD, privateDocD, publicDocD, classDec, dot,
  blockCmtStart, blockCmtEnd, docCmtStart, doubleSlash, elseIfLabel, 
  blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, functionDox, 
  commentedModD, docFuncRepr, valueList, appendToBody, surroundBody, getterName,
  setterName, filterOutObjs)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  fileFromData, block, varDec, varDecDef, listDec, listDecDef, objDecNew, 
  objDecNewNoParams, construct, comment, ifCond, for, while, method, getMethod, 
  setMethod, privMethod, pubMethod, constructor, function, docFunc, 
  docInOutFunc, intFunc, privMVar, pubMVar, pubGVar, privClass, pubClass, 
  docClass, commentedClass, buildModule, modFromData, fileDoc, docMod)
import GOOL.Drasil.Data (Pair(..), Terminator(..), ScopeTag(..), 
  Binding(..), BindData(..), bd, FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateModDoc, OpData(..), od, 
  ParamData(..), pd, ProgData(..), progD, emptyProg, StateVarData(..), svd, 
  TypeData(..), td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.Helpers (angles, doubleQuotedText, emptyIfEmpty, mapPairFst, 
  mapPairSnd, toCode, toState, onCodeValue, onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, liftA4, liftA5, liftA8, 
  liftList, lift2Lists, lift1List, checkParams)
import GOOL.Drasil.State (MS, lensGStoMS, lensMStoGS, initialState, 
  putAfter, getPutReturn, setMain, setCurrMain, getCurrMain, setParameters, 
  setScope, getScope, setCurrMainFunc, getCurrMainFunc)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,pi,const,log,exp,mod)
import Data.Maybe (maybeToList)
import Control.Lens (Lens', over)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad.State (State, evalState)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), braces, parens, comma,
  empty, equals, semi, vcat, lbrace, rbrace, quotes, render, colon, isEmpty)

cppHdrExt, cppSrcExt :: String
cppHdrExt = "hpp"
cppSrcExt = "cpp"

data CppCode x y a = CPPC {src :: x a, hdr :: y a}

instance Pair CppCode where
  pfst (CPPC xa _) = xa
  psnd (CPPC _ yb) = yb
  pair = CPPC

unCPPC :: CppCode CppSrcCode CppHdrCode a -> a
unCPPC (CPPC (CPPSC a) _) = a

hdrToSrc :: CppHdrCode a -> CppSrcCode a
hdrToSrc (CPPHC a) = CPPSC a

instance (Pair p) => ProgramSym (p CppSrcCode CppHdrCode) where
  type Program (p CppSrcCode CppHdrCode) = ProgData
  prog n mods = do
    m <-  mapM (putAfter $ setCurrMain False) mods
    let fm = map pfst m
        sm = map (hdrToSrc . psnd) m
    p1 <- prog n $ map toState sm ++ map toState fm
    toState $ pair p1 (toCode emptyProg)

instance (Pair p) => RenderSym (p CppSrcCode CppHdrCode) where
  type RenderFile (p CppSrcCode CppHdrCode) = FileData
  fileDoc c = pair1 c fileDoc fileDoc

  docMod d a dt m = pair1 m (docMod d a dt) (docMod d a dt)

  commentedMod cmt m = pair2 cmt m commentedMod commentedMod

instance (Pair p) => InternalFile (p CppSrcCode CppHdrCode) where
  top m = pair (top $ pfst m) (top $ psnd m)
  bottom = pair bottom bottom
  
  fileFromData ft fp m = pair1 m (fileFromData ft fp) (fileFromData ft fp)

instance (Pair p) => KeywordSym (p CppSrcCode CppHdrCode) where
  type Keyword (p CppSrcCode CppHdrCode) = Doc
  endStatement = pair endStatement endStatement
  endStatementLoop = pair endStatementLoop endStatementLoop

  include n = pair (include n) (include n)
  inherit n = pair (inherit n) (inherit n)

  list p = pair (list $ pfst p) (list $ psnd p)

  blockStart = pair blockStart blockStart
  blockEnd = pair blockEnd blockEnd

  ifBodyStart = pair ifBodyStart ifBodyStart
  elseIf = pair elseIf elseIf
  
  iterForEachLabel = pair iterForEachLabel iterForEachLabel
  iterInLabel = pair iterInLabel iterInLabel

  commentStart = pair commentStart commentStart
  blockCommentStart = pair blockCommentStart blockCommentStart
  blockCommentEnd = pair blockCommentEnd blockCommentEnd
  docCommentStart = pair docCommentStart docCommentStart
  docCommentEnd = pair docCommentEnd docCommentEnd

  keyDoc k = keyDoc $ pfst k

instance (Pair p) => PermanenceSym (p CppSrcCode CppHdrCode) where
  type Permanence (p CppSrcCode CppHdrCode) = BindData
  static_ = pair static_ static_
  dynamic_ = pair dynamic_ dynamic_

instance (Pair p) => InternalPerm (p CppSrcCode CppHdrCode) where
  permDoc p = permDoc $ pfst p
  binding p = binding $ pfst p

instance (Pair p) => BodySym (p CppSrcCode CppHdrCode) where
  type Body (p CppSrcCode CppHdrCode) = Doc
  body bs = pair (body $ map pfst bs) (body $ map psnd bs)
  bodyStatements sts = pair (bodyStatements $ map pfst sts) (bodyStatements $ 
    map psnd sts)
  oneLiner s = pair (oneLiner $ pfst s) (oneLiner $ psnd s)

  addComments s b = pair (addComments s $ pfst b) (addComments s $ psnd b)

  bodyDoc b = bodyDoc $ pfst b

instance (Pair p) => BlockSym (p CppSrcCode CppHdrCode) where
  type Block (p CppSrcCode CppHdrCode) = Doc
  block sts = pair (block $ map pfst sts) (block $ map psnd sts)

instance (Pair p) => InternalBlock (p CppSrcCode CppHdrCode) where
  blockDoc b = blockDoc $ pfst b
  docBlock d = pair (docBlock d) (docBlock d)

instance (Pair p) => TypeSym (p CppSrcCode CppHdrCode) where
  type Type (p CppSrcCode CppHdrCode) = TypeData
  bool = pair bool bool
  int = pair int int
  float = pair float float
  char = pair char char
  string = pair string string
  infile = pair infile infile
  outfile = pair outfile outfile
  listType p st = pair (listType (pfst p) (pfst st)) (listType (psnd p) 
    (psnd st))
  listInnerType st = pair (listInnerType $ pfst st) (listInnerType $ psnd st)
  obj t = pair (obj t) (obj t)
  enumType t = pair (enumType t) (enumType t)
  iterator t = pair (iterator $ pfst t) (iterator $ psnd t)
  void = pair void void

  getType s = getType $ pfst s
  getTypeString s = getTypeString $ pfst s
  getTypeDoc s = getTypeDoc $ pfst s
  
instance (Pair p) => InternalType (p CppSrcCode CppHdrCode) where
  typeFromData t s d = pair (typeFromData t s d) (typeFromData t s d)

instance (Pair p) => ControlBlockSym (p CppSrcCode CppHdrCode) where
  runStrategy l strats rv av = pair (runStrategy l (map (mapPairSnd pfst) 
    strats) (onCodeValue pfst rv) (onCodeValue pfst av)) (runStrategy l (map 
    (mapPairSnd psnd) strats) (onCodeValue psnd rv) (onCodeValue psnd av))

  listSlice vnew vold b e s = pair (listSlice (pfst vnew) (pfst vold)
    (onCodeValue pfst b) (onCodeValue pfst e) (onCodeValue pfst s)) 
    (listSlice (psnd vnew) (psnd vold) (onCodeValue psnd b) (onCodeValue psnd e)
    (onCodeValue psnd s))

instance (Pair p) => UnaryOpSym (p CppSrcCode CppHdrCode) where
  type UnaryOp (p CppSrcCode CppHdrCode) = OpData
  notOp = pair notOp notOp
  negateOp = pair negateOp negateOp
  sqrtOp = pair sqrtOp sqrtOp
  absOp = pair absOp absOp
  logOp = pair logOp logOp
  lnOp = pair lnOp lnOp
  expOp = pair expOp expOp
  sinOp = pair sinOp sinOp
  cosOp = pair cosOp cosOp
  tanOp = pair tanOp tanOp
  asinOp = pair asinOp asinOp
  acosOp = pair acosOp acosOp
  atanOp = pair atanOp atanOp
  floorOp = pair floorOp floorOp
  ceilOp = pair ceilOp ceilOp

instance (Pair p) => BinaryOpSym (p CppSrcCode CppHdrCode) where
  type BinaryOp (p CppSrcCode CppHdrCode) = OpData
  equalOp = pair equalOp equalOp
  notEqualOp = pair notEqualOp notEqualOp
  greaterOp = pair greaterOp greaterOp
  greaterEqualOp = pair greaterEqualOp greaterEqualOp
  lessOp = pair lessOp lessOp
  lessEqualOp = pair lessEqualOp lessEqualOp
  plusOp = pair plusOp plusOp
  minusOp = pair minusOp minusOp
  multOp = pair multOp multOp
  divideOp = pair divideOp divideOp
  powerOp = pair powerOp powerOp
  moduloOp = pair moduloOp moduloOp
  andOp = pair andOp andOp
  orOp = pair orOp orOp

instance (Pair p) => VariableSym (p CppSrcCode CppHdrCode) where
  type Variable (p CppSrcCode CppHdrCode) = VarData
  var n t = pair (var n $ pfst t) (var n $ psnd t)
  staticVar n t = pair (staticVar n $ pfst t) (staticVar n $ psnd t)
  const n t = pair (const n $ pfst t) (const n $ psnd t)
  extVar l n t = pair (extVar l n $ pfst t) (extVar l n $ psnd t)
  self l = pair (self l) (self l)
  enumVar e en = pair (enumVar e en) (enumVar e en)
  classVar c v = pair (classVar (pfst c) (pfst v)) (classVar (psnd c) (psnd v))
  extClassVar c v = pair (extClassVar (pfst c) (pfst v)) (extClassVar (psnd c) 
    (psnd v))
  objVar o v = pair (objVar (pfst o) (pfst v)) (objVar (psnd o) (psnd v))
  objVarSelf l v = pair (objVarSelf l $ pfst v) (objVarSelf l $ psnd v)
  listVar n p t = pair (listVar n (pfst p) (pfst t)) (listVar n (psnd p) (psnd t))
  n `listOf` t = pair (n `listOf` pfst t) (n `listOf` psnd t)
  iterVar l t = pair (iterVar l $ pfst t) (iterVar l $ psnd t)
  
  ($->) v1 v2 = pair (($->) (pfst v1) (pfst v2)) (($->) (psnd v1) (psnd v2))

  variableBind v = variableBind $ pfst v
  variableName v = variableName $ pfst v
  variableType v = pair (variableType $ pfst v) (variableType $ psnd v)
  variableDoc v = variableDoc $ pfst v

instance (Pair p) => InternalVariable (p CppSrcCode CppHdrCode) where
  varFromData b n t d = pair (varFromData b n (pfst t) d) 
    (varFromData b n (psnd t) d)

instance (Pair p) => ValueSym (p CppSrcCode CppHdrCode) where
  type Value (p CppSrcCode CppHdrCode) = ValData
  litTrue = pair litTrue litTrue
  litFalse = pair litFalse litFalse
  litChar c = pair (litChar c) (litChar c)
  litFloat v = pair (litFloat v) (litFloat v)
  litInt v = pair (litInt v) (litInt v)
  litString s = pair (litString s) (litString s)

  pi = pair pi pi

  ($:) l1 l2 = pair (($:) l1 l2) (($:) l1 l2)

  valueOf v = pair (valueOf $ pfst v) (valueOf $ psnd v)
  arg n = pair (arg n) (arg n)
  enumElement en e = pair (enumElement en e) (enumElement en e)
  
  argsList = pair argsList argsList

  valueType v = pair (valueType $ pfst v) (valueType $ psnd v)
  valueDoc v = valueDoc $ pfst v

instance (Pair p) => NumericExpression (p CppSrcCode CppHdrCode) where
  (#~) v = pair ((#~) $ pfst v) ((#~) $ psnd v)
  (#/^) v = pair ((#/^) $ pfst v) ((#/^) $ psnd v)
  (#|) v = pair ((#|) $ pfst v) ((#|) $ psnd v)
  (#+) v1 v2 = pair ((#+) (pfst v1) (pfst v2)) ((#+) (psnd v1) (psnd v2))
  (#-) v1 v2 = pair ((#-) (pfst v1) (pfst v2)) ((#-) (psnd v1) (psnd v2))
  (#*) v1 v2 = pair ((#*) (pfst v1) (pfst v2)) ((#*) (psnd v1) (psnd v2))
  (#/) v1 v2 = pair ((#/) (pfst v1) (pfst v2)) ((#/) (psnd v1) (psnd v2))
  (#%) v1 v2 = pair ((#%) (pfst v1) (pfst v2)) ((#%) (psnd v1) (psnd v2))
  (#^) v1 v2 = pair ((#^) (pfst v1) (pfst v2)) ((#^) (psnd v1) (psnd v2))

  log v = pair (log $ pfst v) (log $ psnd v)
  ln v = pair (ln $ pfst v) (ln $ psnd v)
  exp v = pair (exp $ pfst v) (exp $ psnd v)
  sin v = pair (sin $ pfst v) (sin $ psnd v)
  cos v = pair (cos $ pfst v) (cos $ psnd v)
  tan v = pair (tan $ pfst v) (tan $ psnd v)
  csc v = pair (csc $ pfst v) (csc $ psnd v)
  sec v = pair (sec $ pfst v) (sec $ psnd v)
  cot v = pair (cot $ pfst v) (cot $ psnd v)
  arcsin v = pair (arcsin $ pfst v) (arcsin $ psnd v)
  arccos v = pair (arccos $ pfst v) (arccos $ psnd v)
  arctan v = pair (arctan $ pfst v) (arctan $ psnd v)
  floor v = pair (floor $ pfst v) (floor $ psnd v)
  ceil v = pair (ceil $ pfst v) (ceil $ psnd v)

instance (Pair p) => BooleanExpression (p CppSrcCode CppHdrCode) where
  (?!) v = pair ((?!) $ pfst v) ((?!) $ psnd v)
  (?&&) v1 v2 = pair ((?&&) (pfst v1) (pfst v2)) ((?&&) (psnd v1) (psnd v2))
  (?||) v1 v2 = pair ((?||) (pfst v1) (pfst v2)) ((?||) (psnd v1) (psnd v2))

  (?<) v1 v2 = pair ((?<) (pfst v1) (pfst v2)) ((?<) (psnd v1) (psnd v2))
  (?<=) v1 v2 = pair ((?<=) (pfst v1) (pfst v2)) ((?<=) (psnd v1) (psnd v2))
  (?>) v1 v2 = pair ((?>) (pfst v1) (pfst v2)) ((?>) (psnd v1) (psnd v2))
  (?>=) v1 v2 = pair ((?>=) (pfst v1) (pfst v2)) ((?>=) (psnd v1) (psnd v2))
  (?==) v1 v2 = pair ((?==) (pfst v1) (pfst v2)) ((?==) (psnd v1) (psnd v2))
  (?!=) v1 v2 = pair ((?!=) (pfst v1) (pfst v2)) ((?!=) (psnd v1) (psnd v2))
  
instance (Pair p) => ValueExpression (p CppSrcCode CppHdrCode) where
  inlineIf b v1 v2 = pair (inlineIf (pfst b) (pfst v1) (pfst v2)) (inlineIf 
    (psnd b) (psnd v1) (psnd v2))
  funcApp n t vs = pair (funcApp n (pfst t) (map pfst vs)) (funcApp n (psnd t) 
    (map psnd vs))
  selfFuncApp c n t vs = pair (selfFuncApp c n (pfst t) (map pfst vs)) 
    (selfFuncApp c n (psnd t) (map psnd vs))
  extFuncApp l n t vs = pair (extFuncApp l n (pfst t) (map pfst vs)) 
    (extFuncApp l n (psnd t) (map psnd vs))
  newObj t vs = pair (newObj (pfst t) (map pfst vs)) (newObj (psnd t) 
    (map psnd vs))
  extNewObj l t vs = pair (extNewObj l (pfst t) (map pfst vs)) 
    (extNewObj l (psnd t) (map psnd vs))

  exists v = pair (exists $ pfst v) (exists $ psnd v)
  notNull v = pair (notNull $ pfst v) (notNull $ psnd v)
  
instance (Pair p) => InternalValue (p CppSrcCode CppHdrCode) where
  inputFunc = pair inputFunc inputFunc
  printFunc = pair printFunc printFunc
  printLnFunc = pair printLnFunc printLnFunc
  printFileFunc v = pair (printFileFunc $ pfst v) (printFileFunc $ psnd v)
  printFileLnFunc v = pair (printFileLnFunc $ pfst v) (printFileLnFunc $ psnd v)

  cast t v = pair (cast (pfst t) (pfst v)) (cast (psnd t) (psnd v))

  valFromData p t d = pair (valFromData p (pfst t) d) (valFromData p (psnd t) d)

instance (Pair p) => Selector (p CppSrcCode CppHdrCode) where
  objAccess v f = pair (objAccess (pfst v) (pfst f)) (objAccess (psnd v) 
    (psnd f))
  ($.) v f = pair (($.) (pfst v) (pfst f)) (($.) (psnd v) (psnd f))

  objMethodCall t o f ps = pair (objMethodCall (pfst t) (pfst o) f 
    (map pfst ps)) (objMethodCall (psnd t) (psnd o) f (map psnd ps))
  objMethodCallNoParams t o f = pair (objMethodCallNoParams (pfst t) (pfst o) f)
    (objMethodCallNoParams (psnd t) (psnd o) f)

  selfAccess l f = pair (selfAccess l $ pfst f) (selfAccess l $ psnd f)

  listIndexExists v i = pair (listIndexExists (pfst v) (pfst i)) 
    (listIndexExists (psnd v) (psnd i))
  argExists i = pair (argExists i) (argExists i)
  
  indexOf l v = pair (indexOf (pfst l) (pfst v)) (indexOf (psnd l) (psnd v))

instance (Pair p) => FunctionSym (p CppSrcCode CppHdrCode) where
  type Function (p CppSrcCode CppHdrCode) = FuncData
  func l t vs = pair (func l (pfst t) (map pfst vs)) (func l (psnd t) (map psnd vs))

  get v vToGet = pair (get (pfst v) (pfst vToGet)) (get (psnd v) (psnd vToGet))
  set v vToSet toVal = pair (set (pfst v) (pfst vToSet) (pfst toVal))
    (set (psnd v) (psnd vToSet) (psnd toVal))

  listSize v = pair (listSize $ pfst v) (listSize $ psnd v)
  listAdd v i vToAdd = pair (listAdd (pfst v) (pfst i) (pfst vToAdd)) 
    (listAdd (psnd v) (psnd i) (psnd vToAdd))
  listAppend v vToApp = pair (listAppend (pfst v) (pfst vToApp)) 
    (listAppend (psnd v) (psnd vToApp))

  iterBegin v = pair (iterBegin $ pfst v) (iterBegin $ psnd v)
  iterEnd v = pair (iterEnd $ pfst v) (iterEnd $ psnd v)

instance (Pair p) => SelectorFunction (p CppSrcCode CppHdrCode) where
  listAccess v i = pair (listAccess (pfst v) (pfst i)) 
    (listAccess (psnd v) (psnd i))
  listSet v i toVal = pair (listSet (pfst v) (pfst i) (pfst toVal)) 
    (listSet (psnd v) (psnd i) (psnd toVal))
  at v i = pair (at (pfst v) (pfst i)) (at (psnd v) (psnd i))

instance (Pair p) => InternalFunction (p CppSrcCode CppHdrCode) where  
  getFunc v = pair (getFunc $ pfst v) (getFunc $ psnd v)
  setFunc t v toVal = pair (setFunc (pfst t) (pfst v) (pfst toVal)) 
    (setFunc (psnd t) (psnd v) (psnd toVal))

  listSizeFunc = pair listSizeFunc listSizeFunc
  listAddFunc l i v = pair (listAddFunc (pfst l) (pfst i) (pfst v)) 
    (listAddFunc (psnd l) (psnd i) (psnd v))
  listAppendFunc v = pair (listAppendFunc $ pfst v) (listAppendFunc $ psnd v)

  iterBeginFunc t = pair (iterBeginFunc $ pfst t) (iterBeginFunc $ psnd t)
  iterEndFunc t = pair (iterEndFunc $ pfst t) (iterEndFunc $ psnd t)

  listAccessFunc t v = pair (listAccessFunc (pfst t) (pfst v)) (listAccessFunc 
    (psnd t) (psnd v))
  listSetFunc v i toVal = pair (listSetFunc (pfst v) (pfst i) (pfst toVal)) 
    (listSetFunc (psnd v) (psnd i) (psnd toVal))

  functionType f = pair (functionType $ pfst f) (functionType $ psnd f)
  functionDoc f = functionDoc $ pfst f
  
  funcFromData t d = pair (funcFromData (pfst t) d) (funcFromData (psnd t) d)

instance (Pair p) => InternalStatement (p CppSrcCode CppHdrCode) where
  printSt nl p v f = pair (printSt nl (pfst p) (pfst v) (onCodeValue pfst f)) 
    (printSt nl (psnd p) (psnd v) (onCodeValue psnd f))
    
  state s = pair (state $ pfst s) (state $ psnd s)
  loopState s = pair (loopState $ pfst s) (loopState $ psnd s)

  emptyState = pair emptyState emptyState
  statementDoc s = statementDoc $ pfst s
  statementTerm s = statementTerm $ pfst s
  
  stateFromData d t = pair (stateFromData d t) (stateFromData d t)

instance (Pair p) => StatementSym (p CppSrcCode CppHdrCode) where
  type Statement (p CppSrcCode CppHdrCode) = (Doc, Terminator)
  assign vr vl = pair (assign (pfst vr) (pfst vl)) (assign (psnd vr) (psnd vl))
  assignToListIndex lst index v = pair (assignToListIndex (pfst lst) (pfst 
    index) (pfst v)) (assignToListIndex (psnd lst) (psnd index) (psnd v))
  multiAssign vrs vls = pair (multiAssign (map pfst vrs) (map pfst vls)) 
    (multiAssign (map psnd vrs) (map psnd vls))
  (&=) vr vl = pair ((&=) (pfst vr) (pfst vl)) ((&=) (psnd vr) (psnd vl))
  (&-=) vr vl = pair ((&-=) (pfst vr) (pfst vl)) ((&-=) (psnd vr) (psnd vl))
  (&+=) vr vl = pair ((&+=) (pfst vr) (pfst vl)) ((&+=) (psnd vr) (psnd vl))
  (&++) v = pair ((&++) $ pfst v) ((&++) $ psnd v)
  (&~-) v = pair ((&~-) $ pfst v) ((&~-) $ psnd v)

  varDec v = pair (varDec $ pfst v) (varDec $ psnd v)
  varDecDef v def = pair (varDecDef (pfst v) (pfst def)) (varDecDef (psnd v) 
    (psnd def))
  listDec n v = pair (listDec n $ pfst v) (listDec n $ psnd v)
  listDecDef v vs = pair (listDecDef (pfst v) (map pfst vs)) (listDecDef 
    (psnd v) (map psnd vs))
  objDecDef v def = pair (objDecDef (pfst v) (pfst def)) (objDecDef (psnd v)
    (psnd def))
  objDecNew v vs = pair (objDecNew (pfst v) (map pfst vs)) (objDecNew 
    (psnd v) (map psnd vs))
  extObjDecNew lib v vs = pair (extObjDecNew lib (pfst v) (map pfst vs)) 
    (extObjDecNew lib (psnd v) (map psnd vs))
  objDecNewNoParams v = pair (objDecNewNoParams $ pfst v) (objDecNewNoParams $ psnd v)
  extObjDecNewNoParams lib v = pair (extObjDecNewNoParams lib $ pfst v) 
    (extObjDecNewNoParams lib $ psnd v)
  constDecDef v def = pair (constDecDef (pfst v) (pfst def)) (constDecDef 
    (psnd v) (psnd def))

  print v = pair (print $ pfst v) (print $ psnd v)
  printLn v = pair (printLn $ pfst v) (printLn $ psnd v)
  printStr s = pair (printStr s) (printStr s)
  printStrLn s = pair (printStrLn s) (printStrLn s)

  printFile f v = pair (printFile (pfst f) (pfst v)) (printFile (psnd f) 
    (psnd v))
  printFileLn f v = pair (printFileLn (pfst f) (pfst v)) (printFileLn (psnd f) 
    (psnd v))
  printFileStr f s = pair (printFileStr (pfst f) s) (printFileStr (psnd f) s)
  printFileStrLn f s = pair (printFileStrLn (pfst f) s) (printFileStrLn (psnd f)
    s)

  getInput v = pair (getInput $ pfst v) (getInput $ psnd v)
  discardInput = pair discardInput discardInput
  getFileInput f v = pair (getFileInput (pfst f) (pfst v)) 
    (getFileInput (psnd f) (psnd v))
  discardFileInput f = pair (discardFileInput $ pfst f) (discardFileInput $
    psnd f)

  openFileR f n = pair (openFileR (pfst f) (pfst n)) 
    (openFileR (psnd f) (psnd n))
  openFileW f n = pair (openFileW (pfst f) (pfst n)) 
    (openFileW (psnd f) (psnd n))
  openFileA f n = pair (openFileA (pfst f) (pfst n)) 
    (openFileA (psnd f) (psnd n))
  closeFile f = pair (closeFile $ pfst f) (closeFile $ psnd f)

  getFileInputLine f v = pair (getFileInputLine (pfst f) (pfst v)) 
    (getFileInputLine (psnd f) (psnd v))
  discardFileLine f = pair (discardFileLine $ pfst f) (discardFileLine $ psnd f)
  stringSplit d vnew s = pair (stringSplit d (pfst vnew) (pfst s)) 
    (stringSplit d (psnd vnew) (psnd s))

  stringListVals vals sl = pair (stringListVals (map pfst vals) (pfst sl))
    (stringListVals (map psnd vals) (psnd sl))
  stringListLists lsts sl = pair (stringListLists (map pfst lsts) (pfst sl))
    (stringListLists (map psnd lsts) (psnd sl))

  break = pair break break
  continue = pair continue continue

  returnState v = pair (returnState $ pfst v) (returnState $ psnd v)
  multiReturn vs = pair (multiReturn $ map pfst vs) (multiReturn $ map psnd vs)

  valState v = pair (valState $ pfst v) (valState $ psnd v)

  comment cmt = pair (comment cmt) (comment cmt)

  free v = pair (free $ pfst v) (free $ psnd v)

  throw errMsg = pair (throw errMsg) (throw errMsg)

  initState fsmName iState = pair (initState fsmName iState) 
    (initState fsmName iState)
  changeState fsmName postState = pair (changeState fsmName postState) 
    (changeState fsmName postState)

  initObserverList t vs = pair (initObserverList (pfst t) (map pfst vs)) 
    (initObserverList (psnd t) (map psnd vs))
  addObserver o = pair (addObserver $ pfst o) (addObserver $ psnd o)

  inOutCall n ins outs both = pair (inOutCall n (map pfst ins) (map pfst outs) 
    (map pfst both)) (inOutCall n (map psnd ins) (map psnd outs) (map psnd both))
  selfInOutCall c n ins outs both = pair (selfInOutCall c n (map pfst ins) (map 
    pfst outs) (map pfst both)) (selfInOutCall c n (map psnd ins) (map psnd 
    outs) (map psnd both))
  extInOutCall m n ins outs both = pair (extInOutCall m n (map pfst ins) (map 
    pfst outs) (map pfst both)) (extInOutCall m n (map psnd ins) (map psnd outs)
    (map psnd both)) 

  multi ss = pair (multi $ map pfst ss) (multi $ map psnd ss)

instance (Pair p) => ControlStatementSym (p CppSrcCode CppHdrCode) where
  ifCond bs b = pair (ifCond (map (mapPairFst pfst . mapPairSnd pfst) bs) 
    (pfst b)) (ifCond (map (mapPairFst psnd . mapPairSnd psnd) bs) (psnd b))
  ifNoElse bs = pair (ifNoElse $ map (mapPairFst pfst . mapPairSnd pfst) bs) 
    (ifNoElse $ map (mapPairFst psnd . mapPairSnd psnd) bs)
  switch v cs c = pair (switch (pfst v) (map (mapPairFst pfst . mapPairSnd pfst)
    cs) (pfst c)) (switch (psnd v) (map (mapPairFst psnd . mapPairSnd psnd) cs)
    (psnd c))
  switchAsIf v cs b = pair (switchAsIf (pfst v) (map 
    (mapPairFst pfst . mapPairSnd pfst) cs) (pfst b)) 
    (switchAsIf (psnd v) (map (mapPairFst psnd . mapPairSnd psnd) cs) (psnd b))

  ifExists cond ifBody elseBody = pair (ifExists (pfst cond) (pfst ifBody)
    (pfst elseBody)) (ifExists (psnd cond) (psnd ifBody) (psnd elseBody))

  for sInit vGuard sUpdate b = pair (for (pfst sInit) (pfst vGuard) (pfst 
    sUpdate) (pfst b)) (for (psnd sInit) (psnd vGuard) (psnd sUpdate) (psnd b))
  forRange i initv finalv stepv b = pair (forRange (pfst i) (pfst initv) 
    (pfst finalv) (pfst stepv) (pfst b)) (forRange (psnd i) (psnd initv) 
    (psnd finalv) (psnd stepv) (psnd b))
  forEach i v b = pair (forEach (pfst i) (pfst v) (pfst b)) (forEach (psnd i) 
    (psnd v) (psnd b))
  while v b = pair (while (pfst v) (pfst b)) (while (psnd v) (psnd b))

  tryCatch tb cb = pair (tryCatch (pfst tb) (pfst cb)) (tryCatch (psnd tb) 
    (psnd cb))

  checkState l vs b = pair (checkState l (map 
    (mapPairFst pfst . mapPairSnd pfst) vs) (pfst b)) 
    (checkState l (map (mapPairFst psnd . mapPairSnd psnd) vs) (psnd b))

  notifyObservers f t = pair (notifyObservers (pfst f) (pfst t)) 
    (notifyObservers (psnd f) (psnd t))

  getFileInputAll f v = pair (getFileInputAll (pfst f) (pfst v)) 
    (getFileInputAll (psnd f) (psnd v))

instance (Pair p) => ScopeSym (p CppSrcCode CppHdrCode) where
  type Scope (p CppSrcCode CppHdrCode) = (Doc, ScopeTag)
  private = pair private private
  public = pair public public

instance (Pair p) => InternalScope (p CppSrcCode CppHdrCode) where
  scopeDoc s = scopeDoc $ pfst s

instance (Pair p) => MethodTypeSym (p CppSrcCode CppHdrCode) where
  type MethodType (p CppSrcCode CppHdrCode) = TypeData
  mType t = pair (mType $ pfst t) (mType $ psnd t)
  construct n = pair (construct n) (construct n)

instance (Pair p) => ParameterSym (p CppSrcCode CppHdrCode) where
  type Parameter (p CppSrcCode CppHdrCode) = ParamData
  param v = pair (param $ pfst v) (param $ psnd v)
  pointerParam v = pair (pointerParam $ pfst v) (pointerParam $ psnd v)

  parameterType p = pair (parameterType $ pfst p) (parameterType $ psnd p)

instance (Pair p) => MethodSym (p CppSrcCode CppHdrCode) where
  type Method (p CppSrcCode CppHdrCode) = MethodData
  method n c s p t ps b = on2StateValues pair (method n c (pfst s) (pfst p) 
    (pfst t) (map pfst ps) (pfst b)) (method n c (psnd s) (psnd p) (psnd t) 
    (map psnd ps) (psnd b))
  getMethod c v = on2StateValues pair (getMethod c $ pfst v) (getMethod c $ 
    psnd v) 
  setMethod c v = on2StateValues pair (setMethod c $ pfst v) (setMethod c $ 
    psnd v)
  privMethod n c t ps b = on2StateValues pair (privMethod n c (pfst t) 
    (map pfst ps) (pfst b)) (privMethod n c (psnd t) (map psnd ps) (psnd b))
  pubMethod n c t ps b = on2StateValues pair (pubMethod n c (pfst t) 
    (map pfst ps) (pfst b)) (pubMethod n c (psnd t) (map psnd ps) (psnd b))
  constructor n ps b = on2StateValues pair (constructor n (map pfst ps) 
    (pfst b)) (constructor n (map psnd ps) (psnd b))
  destructor n vars = pair1List vars lensMStoGS (destructor n) (destructor n)

  docMain b = on2StateValues pair (docMain $ pfst b) (docMain $ psnd b)

  function n s p t ps b = on2StateValues pair (function n (pfst s) (pfst p) 
    (pfst t) (map pfst ps) (pfst b)) (function n (psnd s) (psnd p) (psnd t) 
    (map psnd ps) (psnd b))
  mainFunction b = on2StateValues pair (mainFunction $ pfst b) (mainFunction $ 
    psnd b)

  docFunc desc pComms rComm f = pair1 f (docFunc desc pComms rComm) 
    (docFunc desc pComms rComm)

  inOutMethod n c s p ins outs both b = on2StateValues pair (inOutMethod n c 
    (pfst s) (pfst p) (map pfst ins) (map pfst outs) (map pfst both) (pfst b)) 
    (inOutMethod n c (psnd s) (psnd p) (map psnd ins) (map psnd outs) (map psnd 
    both) (psnd b))

  docInOutMethod n c s p desc is os bs b = on2StateValues pair (docInOutMethod 
    n c (pfst s) (pfst p) desc (map (mapPairSnd pfst) is) (map (mapPairSnd pfst)
    os) (map (mapPairSnd pfst) bs) (pfst b)) (docInOutMethod n c (psnd s) (psnd 
    p) desc (map (mapPairSnd psnd) is) (map (mapPairSnd psnd) os) (map 
    (mapPairSnd psnd) bs) (psnd b))

  inOutFunc n s p ins outs both b = on2StateValues pair (inOutFunc n (pfst s) 
    (pfst p) (map pfst ins) (map pfst outs) (map pfst both) (pfst b)) 
    (inOutFunc n (psnd s) (psnd p) (map psnd ins) (map psnd outs) (map psnd 
    both) (psnd b))

  docInOutFunc n s p desc is os bs b = on2StateValues pair (docInOutFunc n 
    (pfst s) (pfst p) desc (map (mapPairSnd pfst) is) (map (mapPairSnd pfst) os)
    (map (mapPairSnd pfst) bs) (pfst b)) (docInOutFunc n (psnd s) (psnd p) desc 
    (map (mapPairSnd psnd) is) (map (mapPairSnd psnd) os) (map (mapPairSnd psnd)
    bs) (psnd b))

instance (Pair p) => InternalMethod (p CppSrcCode CppHdrCode) where
  intMethod m n c s p t ps b = on2StateValues pair (intMethod m n c (pfst s) 
    (pfst p) (pfst t) (map pfst ps) (pfst b)) (intMethod m n c (psnd s) (psnd p)
    (psnd t) (map psnd ps) (psnd b))
  intFunc m n s p t ps b = on2StateValues pair (intFunc m n (pfst s) (pfst p) 
    (pfst t) (map pfst ps) (pfst b)) (intFunc m n (psnd s) (psnd p) (psnd t) 
    (map psnd ps) (psnd b))
  commentedFunc cmt fn = pair2 cmt fn commentedFunc 
    commentedFunc
    
  methodDoc m = methodDoc $ pfst m
  methodFromData s d = pair (methodFromData s d) (methodFromData s d)

instance (Pair p) => StateVarSym (p CppSrcCode CppHdrCode) where
  type StateVar (p CppSrcCode CppHdrCode) = StateVarData
  stateVar s p v = on2StateValues pair (stateVar (pfst s) (pfst p) (pfst v))
    (stateVar (psnd s) (psnd p) (psnd v))
  stateVarDef n s p vr vl = on2StateValues pair (stateVarDef n (pfst s) (pfst p)
    (pfst vr) (pfst vl)) (stateVarDef n (psnd s) (psnd p) (psnd vr) (psnd vl))
  constVar n s vr vl = on2StateValues pair (constVar n (pfst s) (pfst vr) (pfst 
    vl)) (constVar n (psnd s) (psnd vr) (psnd vl))
  privMVar v = on2StateValues pair (privMVar $ pfst v) (privMVar $ psnd v)
  pubMVar v = on2StateValues pair (pubMVar $ pfst v) (pubMVar $ psnd v)
  pubGVar v = on2StateValues pair (pubGVar $ pfst v) (pubGVar $ psnd v)

instance (Pair p) => InternalStateVar (p CppSrcCode CppHdrCode) where
  stateVarDoc v = stateVarDoc $ pfst v
  stateVarFromData d = on2StateValues pair (stateVarFromData d) 
    (stateVarFromData d)

instance (Pair p) => ClassSym (p CppSrcCode CppHdrCode) where
  type Class (p CppSrcCode CppHdrCode) = Doc
  buildClass n p s vs fs = pair2Lists vs (map (zoom lensGStoMS) fs) 
    (buildClass n p (pfst s)) (buildClass n p (psnd s))
  enum l ls s = on2StateValues pair (enum l ls $ pfst s) (enum l ls $ psnd s)
  privClass n p vs fs = pair2Lists vs (map (zoom lensGStoMS) fs) 
    (privClass n p) (privClass n p)
  pubClass n p vs fs = pair2Lists vs (map (zoom lensGStoMS) fs) 
    (pubClass n p) (pubClass n p)

  docClass d c = pair1 c (docClass d) (docClass d)

  commentedClass cmt cs = pair2 cmt cs commentedClass commentedClass

instance (Pair p) => InternalClass (p CppSrcCode CppHdrCode) where
  classDoc c = classDoc $ pfst c
  classFromData d = on2StateValues pair (classFromData d) (classFromData d)

instance (Pair p) => ModuleSym (p CppSrcCode CppHdrCode) where
  type Module (p CppSrcCode CppHdrCode) = ModData
  buildModule n l ms cs = pair2Lists (map (zoom lensGStoMS . putAfter 
    (setCurrMainFunc False)) ms) cs (buildModule n l) (buildModule n l)
  
instance (Pair p) => InternalMod (p CppSrcCode CppHdrCode) where
  moduleDoc m = moduleDoc $ pfst m
  modFromData n m d = on2StateValues pair (modFromData n m d) (modFromData n m 
    d)
  updateModuleDoc f m = pair1 m (updateModuleDoc f) (updateModuleDoc f)

instance (Pair p) => BlockCommentSym (p CppSrcCode CppHdrCode) where
  type BlockComment (p CppSrcCode CppHdrCode) = Doc
  blockComment lns = pair (blockComment lns) (blockComment lns)
  docComment lns = on2StateValues pair (docComment lns) (docComment lns)

  blockCommentDoc c = blockCommentDoc $ pfst c

-- Helpers for pair instance

pair1 :: (Pair p) => State s (p CppSrcCode CppHdrCode a) -> 
  (State r (CppSrcCode a) -> State s (CppSrcCode b)) -> 
  (State r (CppHdrCode a) -> State s (CppHdrCode b)) -> 
  State s (p CppSrcCode CppHdrCode b)
pair1 stv srcf hdrf = do
  v <- stv
  let fp = toState $ pfst v
      sp = toState $ psnd v
  p1 <- srcf fp
  p2 <- hdrf sp
  toState $ pair p1 p2

pair2 :: (Pair p) => State t (p CppSrcCode CppHdrCode a) -> 
  State t (p CppSrcCode CppHdrCode b) -> 
  (State r (CppSrcCode a) -> State s (CppSrcCode b) -> State t (CppSrcCode c)) 
  -> (State r (CppHdrCode a) -> State s (CppHdrCode b) -> 
  State t (CppHdrCode c)) -> State t (p CppSrcCode CppHdrCode c)
pair2 stv1 stv2 srcf hdrf = do
  v1 <- stv1
  v2 <- stv2
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
      fv2 = toState $ pfst v2
      sv2 = toState $ psnd v2
  p1 <- srcf fv1 fv2
  p2 <- hdrf sv1 sv2
  toState $ pair p1 p2

pair1List :: (Pair p) => [State s (p CppSrcCode CppHdrCode a)] -> Lens' r s -> 
  ([State s (CppSrcCode a)] -> State r (CppSrcCode b)) -> 
  ([State s (CppHdrCode a)] -> State r (CppHdrCode b)) -> 
  State r (p CppSrcCode CppHdrCode b)
pair1List stv l srcf hdrf = do
  v <- mapM (zoom l) stv
  let fl = map (toState . pfst) v
      sl = map (toState . psnd) v
  p1 <- srcf fl
  p2 <- hdrf sl
  toState $ pair p1 p2

pair2Lists :: (Pair p) => [State t (p CppSrcCode CppHdrCode a)] -> 
  [State t (p CppSrcCode CppHdrCode b)] -> ([State r (CppSrcCode a)] -> 
  [State s (CppSrcCode b)] -> State t (CppSrcCode c)) -> 
  ([State r (CppHdrCode a)] -> [State s (CppHdrCode b)] -> 
  State t (CppHdrCode c)) -> State t (p CppSrcCode CppHdrCode c)
pair2Lists stv1 stv2 srcf hdrf = do
  v1 <- sequence stv1
  v2 <- sequence stv2
  let fl1 = map (toState . pfst) v1
      sl1 = map (toState . psnd) v1
      fl2 = map (toState . pfst) v2
      sl2 = map (toState . psnd) v2
  p1 <- srcf fl1 fl2
  p2 <- hdrf sl1 sl2
  toState $ pair p1 p2

-----------------
-- Source File --
-----------------

newtype CppSrcCode a = CPPSC {unCPPSC :: a} deriving Eq

instance Functor CppSrcCode where
  fmap f (CPPSC x) = CPPSC (f x)

instance Applicative CppSrcCode where
  pure = CPPSC
  (CPPSC f) <*> (CPPSC x) = CPPSC (f x)

instance Monad CppSrcCode where
  return = CPPSC
  CPPSC x >>= f = f x

instance ProgramSym CppSrcCode where
  type Program CppSrcCode = ProgData
  prog n = liftList (liftList (progD n))
  
instance RenderSym CppSrcCode where
  type RenderFile CppSrcCode = FileData
  fileDoc code = G.fileDoc Source cppSrcExt (top $ evalState code initialState)
    bottom code

  docMod = G.docMod

  commentedMod cmnt mod = on3StateValues (\m cmt mn -> if mn then on2CodeValues 
    commentedModD m cmt else m) mod cmnt getCurrMain

instance InternalFile CppSrcCode where
  top m = on3CodeValues cppstop m (list dynamic_) endStatement
  bottom = toCode empty
  
  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym CppSrcCode where
  type Keyword CppSrcCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

  include n = toCode $ text "#include" <+> doubleQuotedText (addExt cppHdrExt n)
  inherit n = onCodeValue (cppInherit n . fst) public

  list _ = toCode $ text "vector"

  blockStart = toCode lbrace
  blockEnd = toCode rbrace

  ifBodyStart = blockStart
  elseIf = toCode elseIfLabel
  
  iterForEachLabel = toCode empty
  iterInLabel = toCode empty

  commentStart = toCode doubleSlash
  blockCommentStart = toCode blockCmtStart
  blockCommentEnd = toCode blockCmtEnd
  docCommentStart = toCode docCmtStart
  docCommentEnd = blockCommentEnd

  keyDoc = unCPPSC

instance PermanenceSym CppSrcCode where
  type Permanence CppSrcCode = BindData
  static_ = toCode $ bd Static staticDocD
  dynamic_ = toCode $ bd Dynamic dynamicDocD
  
instance InternalPerm CppSrcCode where
  permDoc = bindDoc . unCPPSC
  binding = bind . unCPPSC

instance BodySym CppSrcCode where
  type Body CppSrcCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner = oneLinerD

  addComments s = on2CodeValues (addCommentsDocD s) commentStart

  bodyDoc = unCPPSC

instance BlockSym CppSrcCode where
  type Block CppSrcCode = Doc
  block = G.block endStatement

instance InternalBlock CppSrcCode where
  blockDoc = unCPPSC
  docBlock = toCode

instance TypeSym CppSrcCode where
  type Type CppSrcCode = TypeData
  bool = toCode cppBoolTypeDoc
  int = toCode intTypeDocD
  float = toCode cppFloatTypeDoc
  char = toCode charTypeDocD
  string = toCode stringTypeDocD
  infile = toCode cppInfileTypeDoc
  outfile = toCode cppOutfileTypeDoc
  listType p st = on2CodeValues listTypeDocD st (list p)
  listInnerType = listInnerTypeD
  obj t = toCode $ typeDocD t
  enumType t = toCode $ enumTypeDocD t
  iterator t = onCodeValue cppIterTypeDoc (listType dynamic_ t)
  void = toCode voidDocD

  getType = cType . unCPPSC
  getTypeString = typeString . unCPPSC
  getTypeDoc = typeDoc . unCPPSC
  
instance InternalType CppSrcCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym CppSrcCode where
  runStrategy = runStrategyD

  listSlice = listSliceD

instance UnaryOpSym CppSrcCode where
  type UnaryOp CppSrcCode = OpData
  notOp = toCode notOpDocD
  negateOp = toCode negateOpDocD
  sqrtOp = toCode sqrtOpDocD
  absOp = toCode absOpDocD
  logOp = toCode $ unOpPrec "log10"
  lnOp = toCode $ unOpPrec "log"
  expOp = toCode expOpDocD
  sinOp = toCode sinOpDocD
  cosOp = toCode cosOpDocD
  tanOp = toCode tanOpDocD
  asinOp = toCode asinOpDocD
  acosOp = toCode acosOpDocD
  atanOp = toCode atanOpDocD
  floorOp = toCode $ unOpPrec "floor"
  ceilOp = toCode $ unOpPrec "ceil"

instance BinaryOpSym CppSrcCode where
  type BinaryOp CppSrcCode = OpData
  equalOp = toCode equalOpDocD
  notEqualOp = toCode notEqualOpDocD
  greaterOp = toCode greaterOpDocD
  greaterEqualOp = toCode greaterEqualOpDocD
  lessOp = toCode lessOpDocD
  lessEqualOp = toCode lessEqualOpDocD
  plusOp = toCode plusOpDocD
  minusOp = toCode minusOpDocD
  multOp = toCode multOpDocD
  divideOp = toCode divideOpDocD
  powerOp = toCode powerOpDocD
  moduloOp = toCode moduloOpDocD
  andOp = toCode andOpDocD
  orOp = toCode orOpDocD

instance VariableSym CppSrcCode where
  type Variable CppSrcCode = VarData
  var = varD
  staticVar = staticVarD
  const = var
  extVar _ = var
  self = selfD
  enumVar = enumVarD
  classVar c v = classVarCheckStatic (varFromData (variableBind v) 
    (getTypeString c ++ "::" ++ variableName v) (variableType v) 
    (cppClassVar (getTypeDoc c) (variableDoc v)))
  extClassVar = classVar
  objVar = objVarD
  objVarSelf _ v = on2CodeValues (mkVar ("this->"++variableName v)) 
    (variableType v) (toCode $ text "this->" <> variableDoc v)
  listVar = listVarD
  listOf = listOfD
  iterVar l t = on2CodeValues (mkVar l) (iterator t) (toCode $ text $ "(*" ++ l 
    ++ ")")

  ($->) = objVar

  variableBind = varBind . unCPPSC
  variableName = varName . unCPPSC
  variableType = onCodeValue varType
  variableDoc = varDoc . unCPPSC

instance InternalVariable CppSrcCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppSrcCode where
  type Value CppSrcCode = ValData
  litTrue = litTrueD
  litFalse = litFalseD
  litChar = litCharD
  litFloat = litFloatD
  litInt = litIntD
  litString = litStringD

  pi = on2CodeValues mkVal float (toCode $ text "M_PI")

  ($:) = enumElement

  valueOf = valueOfD
  arg n = argD (litInt $ n+1) argsList
  enumElement en e = on2CodeValues mkVal (enumType en) (toCode $ text e)
  
  argsList = argsListD "argv"

  valueType = onCodeValue valType
  valueDoc = valDoc . unCPPSC

instance NumericExpression CppSrcCode where
  (#~) = on2CodeValues unExpr' negateOp
  (#/^) = on2CodeValues unExpr sqrtOp
  (#|) = on2CodeValues unExpr absOp
  (#+) = on3CodeValues binExpr plusOp
  (#-) = on3CodeValues binExpr minusOp
  (#*) = on3CodeValues binExpr multOp
  (#/) = on3CodeValues binExpr divideOp
  (#%) = on3CodeValues binExpr moduloOp
  (#^) = on3CodeValues binExpr' powerOp

  log = on2CodeValues unExpr logOp
  ln = on2CodeValues unExpr lnOp
  exp = on2CodeValues unExpr expOp
  sin = on2CodeValues unExpr sinOp
  cos = on2CodeValues unExpr cosOp
  tan = on2CodeValues unExpr tanOp
  csc v = litFloat 1.0 #/ sin v
  sec v = litFloat 1.0 #/ cos v
  cot v = litFloat 1.0 #/ tan v
  arcsin = on2CodeValues unExpr asinOp
  arccos = on2CodeValues unExpr acosOp
  arctan = on2CodeValues unExpr atanOp
  floor = on2CodeValues unExpr floorOp
  ceil = on2CodeValues unExpr ceilOp

instance BooleanExpression CppSrcCode where
  (?!) = on3CodeValues typeUnExpr notOp bool
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = liftA4 typeBinExpr equalOp bool
  (?!=) = liftA4 typeBinExpr notEqualOp bool
   
instance ValueExpression CppSrcCode where
  inlineIf = on3CodeValues inlineIfD
  funcApp = funcAppD
  selfFuncApp c = cppSelfFuncApp (self c)
  extFuncApp _ = funcApp
  newObj = newObjD newObjDocD'
  extNewObj _ = newObj

  exists = notNull
  notNull v = v

instance InternalValue CppSrcCode where
  inputFunc = on2CodeValues mkVal string (toCode $ text "std::cin")
  printFunc = on2CodeValues mkVal void (toCode $ text "std::cout")
  printLnFunc = on2CodeValues mkVal void (toCode $ text "std::cout")
  printFileFunc f = on2CodeValues mkVal void (onCodeValue valDoc f)
  printFileLnFunc f = on2CodeValues mkVal void (onCodeValue valDoc f)

  cast = cppCast
  
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CppSrcCode where
  objAccess = objAccessD
  ($.) = objAccess

  objMethodCall = objMethodCallD
  objMethodCallNoParams = objMethodCallNoParamsD

  selfAccess = selfAccessD

  listIndexExists = listIndexExistsD
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf l v = funcApp "find" int [iterBegin l, iterEnd l, v] #- iterBegin l

instance FunctionSym CppSrcCode where
  type Function CppSrcCode = FuncData
  func = funcD

  get = getD
  set = setD

  listSize v = cast int (listSizeD v)
  listAdd = listAddD
  listAppend = listAppendD

  iterBegin = iterBeginD
  iterEnd = iterEndD

instance SelectorFunction CppSrcCode where
  listAccess = listAccessD
  listSet = listSetD
  at = listAccess

instance InternalFunction CppSrcCode where
  getFunc = getFuncD
  setFunc = setFuncD

  listSizeFunc = listSizeFuncD
  listAddFunc l i v = func "insert" (listType static_ $ valueType v) 
    [iterBegin l #+ i, v]
  listAppendFunc = listAppendFuncD "push_back"

  iterBeginFunc t = func "begin" (iterator t) []
  iterEndFunc t = func "end" (iterator t) []

  listAccessFunc = listAccessFuncD' "at"
  listSetFunc = listSetFuncD cppListSetDoc

  functionType = onCodeValue funcType
  functionDoc = funcDoc . unCPPSC
  
  funcFromData t d = on2CodeValues fd t (toCode d)

instance InternalStatement CppSrcCode where
  printSt nl p v _ = mkSt <$> on2CodeValues (cppPrint nl) p v

  state = stateD
  loopState = loopStateD

  emptyState = emptyStateD
  statementDoc = fst . unCPPSC
  statementTerm = snd . unCPPSC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CppSrcCode where
  type Statement CppSrcCode = (Doc, Terminator)
  assign = assignD Semi
  assignToListIndex = assignToListIndexD
  multiAssign _ _ = error $ multiAssignError cppName
  (&=) = assign
  (&-=) = decrementD
  (&+=) = incrementD
  (&++) = increment1D
  (&~-) = decrement1D

  varDec = G.varDec static_ dynamic_
  varDecDef = G.varDecDef 
  listDec n = G.listDec cppListDecDoc (litInt n)
  listDecDef = G.listDecDef cppListDecDefDoc
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew _ = objDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams _ = objDecNewNoParams
  constDecDef = constDecDefD

  print v = outDoc False printFunc v Nothing
  printLn v = outDoc True printLnFunc v Nothing
  printStr s = outDoc False printFunc (litString s) Nothing
  printStrLn s = outDoc True printLnFunc (litString s) Nothing

  printFile f v = outDoc False (printFileFunc f) v (Just f)
  printFileLn f v = outDoc True (printFileLnFunc f) v (Just f)
  printFileStr f s = outDoc False (printFileFunc f) (litString s) (Just f)
  printFileStrLn f s = outDoc True (printFileLnFunc f) (litString s) (Just f)

  getInput v = mkSt <$> on3CodeValues cppInput v inputFunc endStatement
  discardInput = discardInputD (cppDiscardInput "\\n")
  getFileInput f v = mkSt <$> on3CodeValues cppInput v f endStatement
  discardFileInput = discardFileInputD (cppDiscardInput " ")

  openFileR f n = mkSt <$> on2CodeValues (cppOpenFile "std::fstream::in") f n
  openFileW f n = mkSt <$> on2CodeValues (cppOpenFile "std::fstream::out") f n
  openFileA f n = mkSt <$> on2CodeValues (cppOpenFile "std::fstream::app") f n
  closeFile = closeFileD "close"

  getFileInputLine f v = valState $ funcApp "std::getline" string [f, valueOf v]
  discardFileLine f = mkSt <$> toCode (cppDiscardInput "\\n" f)
  stringSplit d vnew s = let l_ss = "ss"
                             var_ss = var l_ss (obj "std::stringstream")
                             v_ss = valueOf var_ss
                             l_word = "word"
                             var_word = var l_word string
                             v_word = valueOf var_word
                         in
    multi [
      valState $ valueOf vnew $. func "clear" void [],
      varDec var_ss,
      valState $ objMethodCall string v_ss "str" [s],
      varDec var_word,
      while (funcApp "std::getline" string [v_ss, v_word, litChar d]) 
        (oneLiner $ valState $ listAppend (valueOf vnew) v_word)
    ]

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = breakD Semi
  continue = continueD Semi

  returnState = returnD Semi
  multiReturn _ = error $ multiReturnError cppName

  valState = valStateD Semi

  comment = G.comment commentStart

  free v = mkSt <$> onCodeValue freeDocD v

  throw = throwD cppThrowDoc Semi

  initState = initStateD
  changeState = changeStateD

  initObserverList = initObserverListD
  addObserver = addObserverD

  inOutCall = cppInOutCall funcApp
  selfInOutCall c = cppInOutCall (selfFuncApp c)
  extInOutCall m = cppInOutCall (extFuncApp m)

  multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CppSrcCode where
  ifCond = G.ifCond ifBodyStart elseIf blockEnd
  ifNoElse = ifNoElseD
  switch = switchD
  switchAsIf = switchAsIfD

  ifExists _ ifBody _ = mkStNoEnd <$> ifBody -- All variables are initialized in C++

  for = G.for blockStart blockEnd 
  forRange = forRangeD
  forEach i v = for (varDecDef e (iterBegin v)) (valueOf e ?!= iterEnd v) 
    (e &++)
    where e = toBasicVar i
  while = G.while blockStart blockEnd

  tryCatch = tryCatchD cppTryCatch

  checkState l = switchAsIf (valueOf $ var l string) 
  notifyObservers = notifyObserversD

  getFileInputAll f v = let l_line = "nextLine"
                            var_line = var l_line string
                            v_line = valueOf var_line
                        in
    multi [varDec var_line,
      while (funcApp "std::getline" string [f, v_line])
      (oneLiner $ valState $ listAppend (valueOf v) v_line)]

instance ScopeSym CppSrcCode where
  type Scope CppSrcCode = (Doc, ScopeTag)
  private = toCode (privateDocD, Priv)
  public = toCode (publicDocD, Pub)

instance InternalScope CppSrcCode where
  scopeDoc = fst . unCPPSC

instance MethodTypeSym CppSrcCode where
  type MethodType CppSrcCode = TypeData
  mType t = t
  construct = toCode . G.construct

instance ParameterSym CppSrcCode where
  type Parameter CppSrcCode = ParamData
  param = onCodeValue (mkParam paramDocD)
  pointerParam = onCodeValue (mkParam cppPointerParamDoc)

  parameterType = variableType . onCodeValue paramVar

instance MethodSym CppSrcCode where
  type Method CppSrcCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor n = G.constructor n n
  destructor n vs = 
    let i = var "i" int
        -- temporary evalState until I add more state
        deleteStatements = map (onCodeValue destructSts . (`evalState` 
          initialState)) vs
        loopIndexDec = varDec i
        dbody = on2CodeValues emptyIfEmpty 
          (onCodeValue vcat (mapM (onCodeValue fst) deleteStatements)) $
          bodyStatements $ loopIndexDec : deleteStatements
    in pubMethod ('~':n) n void [] dbody

  docMain b = commentedFunc (docComment $ toState $ functionDox 
    "Controls the flow of the program" 
    [("argc", "Number of command-line arguments"),
    ("argv", "List of command-line arguments")] ["exit code"]) (mainFunction b)

  function = G.function
  mainFunction b = intFunc True "main" public static_ (mType int) 
    [param $ var "argc" int, 
    on2CodeValues pd (var "argv" (listType static_ string)) 
    (toCode $ text "const char *argv[]")] 
    (on2CodeValues appendToBody b (returnState $ litInt 0))

  docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

  inOutMethod n c = cppsInOut (method n c)

  docInOutMethod n c = G.docInOutFunc (inOutMethod n c)

  inOutFunc n = cppsInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CppSrcCode where
  intMethod m n c s _ t ps b = getPutReturn (setScope (snd $ unCPPSC s) .
    setParameters (map unCPPSC ps) . if m then over lensMStoGS (setCurrMain m) 
    . setMain else id) $ on2CodeValues mthd (onCodeValue snd s) (liftA5 
    (cppsMethod n c) t (liftList (paramListDocD . checkParams n) ps) b 
    blockStart blockEnd)
  intFunc m n s _ t ps b = getPutReturn (setScope (snd $ unCPPSC s) . 
    setParameters (map unCPPSC ps) . if m then setCurrMainFunc m . over
    lensMStoGS (setCurrMain m) . setMain else id) $ on2CodeValues mthd 
    (onCodeValue snd s) (liftA5 (cppsFunction n) t (liftList (paramListDocD . 
    checkParams n) ps) b blockStart blockEnd)
  commentedFunc = cppCommentedFunc Source
 
  methodDoc = mthdDoc . unCPPSC
  methodFromData s d = toCode $ mthd s d

instance StateVarSym CppSrcCode where
  type StateVar CppSrcCode = StateVarData
  stateVar s _ _ = toState $ on3CodeValues svd (onCodeValue snd s) (toCode 
    empty) emptyState
  stateVarDef n s p vr vl = toState $ on3CodeValues svd (onCodeValue snd s) 
    (liftA4 (cppsStateVarDef n empty) p vr vl endStatement) emptyState
  constVar n s vr vl = toState $ on3CodeValues svd (onCodeValue snd s) (liftA4 
    (cppsStateVarDef n (text "const")) static_ vr vl endStatement) emptyState
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar CppSrcCode where
  stateVarDoc = stVarDoc . unCPPSC
  stateVarFromData = error "stateVarFromData unimplemented in C++"

instance ClassSym CppSrcCode where
  type Class CppSrcCode = Doc
  buildClass n _ _ vs fs = lift2Lists (lift2Lists cppsClass) vs
    (map (zoom lensGStoMS) $ fs ++ [destructor n vs])
  enum _ _ _ = toState $ toCode empty
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass _ cs = cs

instance InternalClass CppSrcCode where
  classDoc = unCPPSC
  classFromData = onStateValue toCode

instance ModuleSym CppSrcCode where
  type Module CppSrcCode = ModData
  buildModule n ls = G.buildModule n (map include ls)

instance InternalMod CppSrcCode where
  moduleDoc = modDoc . unCPPSC
  modFromData n = G.modFromData n (\d m -> toCode $ md n m d)
  updateModuleDoc f = onStateValue (onCodeValue (updateModDoc f))

instance BlockCommentSym CppSrcCode where
  type BlockComment CppSrcCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCPPSC

-----------------
-- Header File --
-----------------

newtype CppHdrCode a = CPPHC {unCPPHC :: a} deriving Eq

instance Functor CppHdrCode where
  fmap f (CPPHC x) = CPPHC (f x)

instance Applicative CppHdrCode where
  pure = CPPHC
  (CPPHC f) <*> (CPPHC x) = CPPHC (f x)

instance Monad CppHdrCode where
  return = CPPHC
  CPPHC x >>= f = f x

instance RenderSym CppHdrCode where
  type RenderFile CppHdrCode = FileData
  fileDoc code = G.fileDoc Header cppHdrExt (top $ evalState code initialState)
    bottom code
  
  docMod = G.docMod

  commentedMod cmnt mod = on3StateValues (\m cmt mn -> if mn then m else on2CodeValues 
    commentedModD m cmt) mod cmnt getCurrMain

instance InternalFile CppHdrCode where
  top m = on3CodeValues cpphtop m (list dynamic_) endStatement
  bottom = toCode $ text "#endif"
  
  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym CppHdrCode where
  type Keyword CppHdrCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

  include n = toCode $ text "#include" <+> doubleQuotedText (addExt cppHdrExt n)
  inherit n = onCodeValue (cppInherit n . fst) public

  list _ = toCode $ text "vector"

  blockStart = toCode lbrace
  blockEnd = toCode rbrace

  ifBodyStart = toCode empty
  elseIf = toCode empty
  
  iterForEachLabel = toCode empty
  iterInLabel = toCode empty

  commentStart = toCode empty
  blockCommentStart = toCode blockCmtStart
  blockCommentEnd = toCode blockCmtEnd
  docCommentStart = toCode docCmtStart
  docCommentEnd = blockCommentEnd

  keyDoc = unCPPHC

instance PermanenceSym CppHdrCode where
  type Permanence CppHdrCode = BindData
  static_ = toCode $ bd Static staticDocD
  dynamic_ = toCode $ bd Dynamic dynamicDocD

instance InternalPerm CppHdrCode where
  permDoc = bindDoc . unCPPHC
  binding = bind . unCPPHC

instance BodySym CppHdrCode where
  type Body CppHdrCode = Doc
  body _ = toCode empty
  bodyStatements _ = toCode empty
  oneLiner _ = toCode empty

  addComments _ _ = toCode empty

  bodyDoc = unCPPHC

instance BlockSym CppHdrCode where
  type Block CppHdrCode = Doc
  block _ = toCode empty

instance InternalBlock CppHdrCode where
  blockDoc = unCPPHC
  docBlock = toCode

instance TypeSym CppHdrCode where
  type Type CppHdrCode = TypeData
  bool = toCode cppBoolTypeDoc
  int = toCode intTypeDocD
  float = toCode cppFloatTypeDoc
  char = toCode charTypeDocD
  string = toCode stringTypeDocD
  infile = toCode cppInfileTypeDoc
  outfile = toCode cppOutfileTypeDoc
  listType p st = on2CodeValues listTypeDocD st (list p)
  listInnerType = listInnerTypeD
  obj t = toCode $ typeDocD t
  enumType t = toCode $ enumTypeDocD t
  iterator t = onCodeValue cppIterTypeDoc (listType dynamic_ t)
  void = toCode voidDocD

  getType = cType . unCPPHC
  getTypeString = typeString . unCPPHC
  getTypeDoc = typeDoc . unCPPHC
  
instance InternalType CppHdrCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym CppHdrCode where
  runStrategy _ _ _ _ = toCode empty

  listSlice _ _ _ _ _ = toCode empty

instance UnaryOpSym CppHdrCode where
  type UnaryOp CppHdrCode = OpData
  notOp = toCode $ od 0 empty
  negateOp = toCode $ od 0 empty
  sqrtOp = toCode $ od 0 empty
  absOp = toCode $ od 0 empty
  logOp = toCode $ od 0 empty
  lnOp = toCode $ od 0 empty
  expOp = toCode $ od 0 empty
  sinOp = toCode $ od 0 empty
  cosOp = toCode $ od 0 empty
  tanOp = toCode $ od 0 empty
  asinOp = toCode $ od 0 empty
  acosOp = toCode $ od 0 empty
  atanOp = toCode $ od 0 empty
  floorOp = toCode $ od 0 empty
  ceilOp = toCode $ od 0 empty

instance BinaryOpSym CppHdrCode where
  type BinaryOp CppHdrCode = OpData
  equalOp = toCode $ od 0 empty
  notEqualOp = toCode $ od 0 empty
  greaterOp = toCode $ od 0 empty
  greaterEqualOp = toCode $ od 0 empty
  lessOp = toCode $ od 0 empty
  lessEqualOp = toCode $ od 0 empty
  plusOp = toCode $ od 0 empty
  minusOp = toCode $ od 0 empty
  multOp = toCode $ od 0 empty
  divideOp = toCode $ od 0 empty
  powerOp = toCode $ od 0 empty
  moduloOp = toCode $ od 0 empty
  andOp = toCode $ od 0 empty
  orOp = toCode $ od 0 empty

instance VariableSym CppHdrCode where
  type Variable CppHdrCode = VarData
  var = varD 
  staticVar = staticVarD
  const _ _ = on2CodeValues (mkVar "") void (toCode empty)
  extVar _ _ _ = on2CodeValues (mkVar "") void (toCode empty)
  self _ = on2CodeValues (mkVar "") void (toCode empty)
  enumVar _ _ = on2CodeValues (mkVar "") void (toCode empty)
  classVar _ _ = on2CodeValues (mkVar "") void (toCode empty)
  extClassVar _ _ = on2CodeValues (mkVar "") void (toCode empty)
  objVar _ _ = on2CodeValues (mkVar "") void (toCode empty)
  objVarSelf _ _ = on2CodeValues (mkVar "") void (toCode empty)
  listVar _ _ _ = on2CodeValues (mkVar "") void (toCode empty)
  listOf _ _ = on2CodeValues (mkVar "") void (toCode empty)
  iterVar _ _ = on2CodeValues (mkVar "") void (toCode empty)

  ($->) _ _ = on2CodeValues (mkVar "") void (toCode empty)
  
  variableBind = varBind . unCPPHC
  variableName = varName . unCPPHC
  variableType = onCodeValue varType
  variableDoc = varDoc . unCPPHC

instance InternalVariable CppHdrCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppHdrCode where
  type Value CppHdrCode = ValData
  litTrue = litTrueD
  litFalse = litFalseD
  litChar = litCharD
  litFloat = litFloatD
  litInt = litIntD
  litString = litStringD

  pi = on2CodeValues mkVal float (toCode $ text "M_PI")

  ($:) = enumElement

  valueOf = valueOfD
  arg n = argD (litInt $ n+1) argsList
  enumElement en e = on2CodeValues mkVal (enumType en) (toCode $ text e)
  
  argsList = argsListD "argv"

  valueType = onCodeValue valType
  valueDoc = valDoc . unCPPHC

instance NumericExpression CppHdrCode where
  (#~) _ = on2CodeValues mkVal void (toCode empty)
  (#/^) _ = on2CodeValues mkVal void (toCode empty)
  (#|) _ = on2CodeValues mkVal void (toCode empty)
  (#+) _ _ = on2CodeValues mkVal void (toCode empty)
  (#-) _ _ = on2CodeValues mkVal void (toCode empty)
  (#*) _ _ = on2CodeValues mkVal void (toCode empty)
  (#/) _ _ = on2CodeValues mkVal void (toCode empty)
  (#%) _ _ = on2CodeValues mkVal void (toCode empty)
  (#^) _ _ = on2CodeValues mkVal void (toCode empty)

  log _ = on2CodeValues mkVal void (toCode empty)
  ln _ = on2CodeValues mkVal void (toCode empty)
  exp _ = on2CodeValues mkVal void (toCode empty)
  sin _ = on2CodeValues mkVal void (toCode empty)
  cos _ = on2CodeValues mkVal void (toCode empty)
  tan _ = on2CodeValues mkVal void (toCode empty)
  csc _ = on2CodeValues mkVal void (toCode empty)
  sec _ = on2CodeValues mkVal void (toCode empty)
  cot _ = on2CodeValues mkVal void (toCode empty)
  arcsin _ = on2CodeValues mkVal void (toCode empty)
  arccos _ = on2CodeValues mkVal void (toCode empty)
  arctan _ = on2CodeValues mkVal void (toCode empty)
  floor _ = on2CodeValues mkVal void (toCode empty)
  ceil _ = on2CodeValues mkVal void (toCode empty)

instance BooleanExpression CppHdrCode where
  (?!) _ = on2CodeValues mkVal void (toCode empty)
  (?&&) _ _ = on2CodeValues mkVal void (toCode empty)
  (?||) _ _ = on2CodeValues mkVal void (toCode empty)

  (?<) _ _ = on2CodeValues mkVal void (toCode empty)
  (?<=) _ _ = on2CodeValues mkVal void (toCode empty)
  (?>) _ _ = on2CodeValues mkVal void (toCode empty)
  (?>=) _ _ = on2CodeValues mkVal void (toCode empty)
  (?==) _ _ = on2CodeValues mkVal void (toCode empty)
  (?!=) _ _ = on2CodeValues mkVal void (toCode empty)
   
instance ValueExpression CppHdrCode where
  inlineIf _ _ _ = on2CodeValues mkVal void (toCode empty)
  funcApp _ _ _ = on2CodeValues mkVal void (toCode empty)
  selfFuncApp _ _ _ _ = on2CodeValues mkVal void (toCode empty)
  extFuncApp _ _ _ _ = on2CodeValues mkVal void (toCode empty)
  newObj _ _ = on2CodeValues mkVal void (toCode empty)
  extNewObj _ _ _ = on2CodeValues mkVal void (toCode empty)

  exists _ = on2CodeValues mkVal void (toCode empty)
  notNull _ = on2CodeValues mkVal void (toCode empty)

instance InternalValue CppHdrCode where
  inputFunc = on2CodeValues mkVal void (toCode empty)
  printFunc = on2CodeValues mkVal void (toCode empty)
  printLnFunc = on2CodeValues mkVal void (toCode empty)
  printFileFunc _ = on2CodeValues mkVal void (toCode empty)
  printFileLnFunc _ = on2CodeValues mkVal void (toCode empty)
  
  cast _ _ = on2CodeValues mkVal void (toCode empty)
  
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CppHdrCode where
  objAccess _ _ = on2CodeValues mkVal void (toCode empty)
  ($.) _ _ = on2CodeValues mkVal void (toCode empty)

  objMethodCall _ _ _ _ = on2CodeValues mkVal void (toCode empty)
  objMethodCallNoParams _ _ _ = on2CodeValues mkVal void (toCode empty)

  selfAccess _ _ = on2CodeValues mkVal void (toCode empty)

  listIndexExists _ _ = on2CodeValues mkVal void (toCode empty)
  argExists _ = on2CodeValues mkVal void (toCode empty)
  
  indexOf _ _ = on2CodeValues mkVal void (toCode empty)

instance FunctionSym CppHdrCode where
  type Function CppHdrCode = FuncData
  func _ _ _ = on2CodeValues fd void (toCode empty)
  
  get _ _ = on2CodeValues mkVal void (toCode empty)
  set _ _ _ = on2CodeValues mkVal void (toCode empty)

  listSize _ = on2CodeValues mkVal void (toCode empty)
  listAdd _ _ _ = on2CodeValues mkVal void (toCode empty)
  listAppend _ _ = on2CodeValues mkVal void (toCode empty)

  iterBegin _ = on2CodeValues mkVal void (toCode empty)
  iterEnd _ = on2CodeValues mkVal void (toCode empty)

instance SelectorFunction CppHdrCode where
  listAccess _ _ = on2CodeValues mkVal void (toCode empty)
  listSet _ _ _ = on2CodeValues mkVal void (toCode empty)
  at _ _ = on2CodeValues mkVal void (toCode empty)

instance InternalFunction CppHdrCode where
  getFunc _ = on2CodeValues fd void (toCode empty)
  setFunc _ _ _ = on2CodeValues fd void (toCode empty)

  listSizeFunc = on2CodeValues fd void (toCode empty)
  listAddFunc _ _ _ = on2CodeValues fd void (toCode empty)
  listAppendFunc _ = on2CodeValues fd void (toCode empty)

  iterBeginFunc _ = on2CodeValues fd void (toCode empty)
  iterEndFunc _ = on2CodeValues fd void (toCode empty)

  listAccessFunc _ _ = on2CodeValues fd void (toCode empty)
  listSetFunc _ _ _ = on2CodeValues fd void (toCode empty)
  
  functionType = onCodeValue funcType
  functionDoc = funcDoc . unCPPHC
  
  funcFromData t d = on2CodeValues fd t (toCode d)

instance InternalStatement CppHdrCode where
  printSt _ _ _ _ = toCode (mkStNoEnd empty)

  state = stateD
  loopState _ = toCode (mkStNoEnd empty)

  emptyState = toCode $ mkStNoEnd empty
  statementDoc = fst . unCPPHC
  statementTerm = snd . unCPPHC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CppHdrCode where
  type Statement CppHdrCode = (Doc, Terminator)
  assign _ _ = toCode (mkStNoEnd empty)
  assignToListIndex _ _ _ = toCode (mkStNoEnd empty)
  multiAssign _ _ = toCode (mkStNoEnd empty)
  (&=) _ _ = toCode (mkStNoEnd empty)
  (&-=) _ _ = toCode (mkStNoEnd empty)
  (&+=) _ _ = toCode (mkStNoEnd empty)
  (&++) _ = toCode (mkStNoEnd empty)
  (&~-) _ = toCode (mkStNoEnd empty)

  varDec = G.varDec static_ dynamic_
  varDecDef = G.varDecDef
  listDec _ _ = toCode (mkStNoEnd empty)
  listDecDef _ _ = toCode (mkStNoEnd empty)
  objDecDef _ _ = toCode (mkStNoEnd empty)
  objDecNew _ _ = toCode (mkStNoEnd empty)
  extObjDecNew _ _ _ = toCode (mkStNoEnd empty)
  objDecNewNoParams _ = toCode (mkStNoEnd empty)
  extObjDecNewNoParams _ _ = toCode (mkStNoEnd empty)
  constDecDef = constDecDefD

  print _ = toCode (mkStNoEnd empty)
  printLn _ = toCode (mkStNoEnd empty)
  printStr _ = toCode (mkStNoEnd empty)
  printStrLn _ = toCode (mkStNoEnd empty)

  printFile _ _ = toCode (mkStNoEnd empty)
  printFileLn _ _ = toCode (mkStNoEnd empty)
  printFileStr _ _ = toCode (mkStNoEnd empty)
  printFileStrLn _ _ = toCode (mkStNoEnd empty)

  getInput _ = toCode (mkStNoEnd empty)
  discardInput = toCode (mkStNoEnd empty)
  getFileInput _ _ = toCode (mkStNoEnd empty)
  discardFileInput _ = toCode (mkStNoEnd empty)

  openFileR _ _ = toCode (mkStNoEnd empty)
  openFileW _ _ = toCode (mkStNoEnd empty)
  openFileA _ _ = toCode (mkStNoEnd empty)
  closeFile _ = toCode (mkStNoEnd empty)

  getFileInputLine _ _ = toCode (mkStNoEnd empty)
  discardFileLine _ = toCode (mkStNoEnd empty)
  stringSplit _ _ _ = toCode (mkStNoEnd empty)

  stringListVals _ _ = toCode (mkStNoEnd empty)
  stringListLists _ _ = toCode (mkStNoEnd empty)

  break = toCode (mkStNoEnd empty)
  continue = toCode (mkStNoEnd empty)

  returnState _ = toCode (mkStNoEnd empty)
  multiReturn _ = toCode (mkStNoEnd empty)

  valState _ = toCode (mkStNoEnd empty)

  comment _ = toCode (mkStNoEnd empty)

  free _ = toCode (mkStNoEnd empty)

  throw _ = toCode (mkStNoEnd empty)

  initState _ _ = toCode (mkStNoEnd empty)
  changeState _ _ = toCode (mkStNoEnd empty)

  initObserverList _ _ = toCode (mkStNoEnd empty)
  addObserver _ = toCode (mkStNoEnd empty)

  inOutCall _ _ _ _ = toCode (mkStNoEnd empty)
  selfInOutCall _ _ _ _ _ = toCode (mkStNoEnd empty)
  extInOutCall _ _ _ _ _ = toCode (mkStNoEnd empty)

  multi _ = toCode (mkStNoEnd empty)

instance ControlStatementSym CppHdrCode where
  ifCond _ _ = toCode (mkStNoEnd empty)
  ifNoElse _ = toCode (mkStNoEnd empty)
  switch _ _ _ = toCode (mkStNoEnd empty)
  switchAsIf _ _ _ = toCode (mkStNoEnd empty)

  ifExists _ _ _ = toCode (mkStNoEnd empty)

  for _ _ _ _ = toCode (mkStNoEnd empty)
  forRange _ _ _ _ _ = toCode (mkStNoEnd empty)
  forEach _ _ _ = toCode (mkStNoEnd empty)
  while _ _ = toCode (mkStNoEnd empty)

  tryCatch _ _ = toCode (mkStNoEnd empty)

  checkState _ _ _ = toCode (mkStNoEnd empty)

  notifyObservers _ _ = toCode (mkStNoEnd empty)

  getFileInputAll _ _ = toCode (mkStNoEnd empty)

instance ScopeSym CppHdrCode where
  type Scope CppHdrCode = (Doc, ScopeTag)
  private = toCode (privateDocD, Priv)
  public = toCode (publicDocD, Pub)

instance InternalScope CppHdrCode where
  scopeDoc = fst . unCPPHC

instance MethodTypeSym CppHdrCode where
  type MethodType CppHdrCode = TypeData
  mType t = t
  construct = toCode . G.construct

instance ParameterSym CppHdrCode where
  type Parameter CppHdrCode = ParamData
  param = onCodeValue (mkParam paramDocD)
  pointerParam = onCodeValue (mkParam cppPointerParamDoc)

  parameterType = variableType . onCodeValue paramVar

instance MethodSym CppHdrCode where
  type Method CppHdrCode = MethodData
  method = G.method
  getMethod c v = method (getterName $ variableName v) c public dynamic_ 
    (variableType v) [] (toCode empty)
  setMethod c v = method (setterName $ variableName v) c public dynamic_ void 
    [param v] (toCode empty)
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor n = G.constructor n n
  destructor n vars = lift1List (\m vs -> toCode $ mthd Pub 
    (emptyIfEmpty (vcat (map (statementDoc . onCodeValue destructSts) vs)) 
    (methodDoc m))) (pubMethod ('~':n) n void [] (toCode empty) :: MS (CppHdrCode (Method CppHdrCode))) (map (zoom lensMStoGS) vars)

  docMain = mainFunction

  function = G.function
  mainFunction _ = getPutReturn (setScope Pub) $ toCode $ mthd Pub empty

  docFunc = G.docFunc

  inOutMethod n c = cpphInOut (method n c)

  docInOutMethod n c = G.docInOutFunc (inOutMethod n c)

  inOutFunc n = cpphInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CppHdrCode where
  intMethod m n _ s _ t ps _ = getPutReturn (setScope (snd $ unCPPHC s) . 
    setParameters (map unCPPHC ps) . if m then over lensMStoGS (setCurrMain m) 
    . setMain else id) $ on2CodeValues mthd (onCodeValue snd s) (on3CodeValues 
    (cpphMethod n) t (liftList (paramListDocD . checkParams n) ps) endStatement)
  intFunc = G.intFunc
  commentedFunc = cppCommentedFunc Header

  methodDoc = mthdDoc . unCPPHC
  methodFromData s d = toCode $ mthd s d

instance StateVarSym CppHdrCode where
  type StateVar CppHdrCode = StateVarData
  stateVar s p v = toState $ on3CodeValues svd (onCodeValue snd s) (toCode $ 
    stateVarDocD empty (permDoc p) (statementDoc (state $ varDec v))) emptyState
  stateVarDef _ s p vr vl = toState $ on3CodeValues svd (onCodeValue snd s) 
    (toCode $ cpphStateVarDef empty p vr vl) emptyState
  constVar _ s v _ = toState $ on3CodeValues svd (onCodeValue snd s) 
    (on3CodeValues (constVarDocD empty) (bindDoc <$> static_) v endStatement) 
    emptyState
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar CppHdrCode where
  stateVarDoc = stVarDoc . unCPPHC
  stateVarFromData = error "stateVarFromData unimplemented in C++"

instance ClassSym CppHdrCode where
  type Class CppHdrCode = Doc
  -- do this with a do? avoids liftA8...
  buildClass n p _ vs mths = lift2Lists (\vars funcs -> liftA8 (cpphClass n) 
    (lift2Lists (cpphVarsFuncsList Pub) vars funcs) 
    (lift2Lists (cpphVarsFuncsList Priv) vars funcs) 
    (onCodeValue fst public) (onCodeValue fst private) parent blockStart 
    blockEnd endStatement) vs fs
    where parent = case p of Nothing -> toCode empty
                             Just pn -> inherit pn
          fs = map (zoom lensGStoMS) $ mths ++ [destructor n vs]
  enum n es _ = toState $ liftA4 (cpphEnum n) (toCode $ enumElementsDocD es 
    enumsEqualInts) blockStart blockEnd endStatement
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance InternalClass CppHdrCode where
  classDoc = unCPPHC
  classFromData = onStateValue toCode

instance ModuleSym CppHdrCode where
  type Module CppHdrCode = ModData
  buildModule n ls = G.buildModule n (map include ls)

instance InternalMod CppHdrCode where
  moduleDoc = modDoc . unCPPHC
  modFromData n = G.modFromData n (\d m -> toCode $ md n m d)
  updateModuleDoc f = onStateValue (onCodeValue (updateModDoc f))

instance BlockCommentSym CppHdrCode where
  type BlockComment CppHdrCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCPPHC

-- helpers
toBasicVar :: CppSrcCode (Variable CppSrcCode) -> 
  CppSrcCode (Variable CppSrcCode)
toBasicVar v = var (variableName v) (variableType v)

isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

getParam :: VarData -> ParamData
getParam v = mkParam (getParamFunc ((cType . varType) v)) v
  where getParamFunc (List _) = cppPointerParamDoc
        getParamFunc (Object _) = cppPointerParamDoc
        getParamFunc _ = paramDocD
 
data MethodData = MthD {getMthdScp :: ScopeTag, mthdDoc :: Doc}

mthd :: ScopeTag -> Doc -> MethodData
mthd = MthD 

-- convenience
cppName :: String
cppName = "C++" 

enumsEqualInts :: Bool
enumsEqualInts = False

inc :: Doc
inc = text "#include"

cppstop :: ModData -> Doc -> Doc -> Doc
cppstop m lst end = vcat [
  if b then empty else inc <+> doubleQuotedText (addExt cppHdrExt n),
  if b then empty else blank,
  text "#define" <+> text "_USE_MATH_DEFINES", --FIXME: Only include if used (i.e. pi)
  inc <+> angles (text "algorithm"),
  inc <+> angles (text "iostream"),
  inc <+> angles (text "fstream"),
  inc <+> angles (text "iterator"),
  inc <+> angles (text "string"),
  inc <+> angles (text "math.h"),
  inc <+> angles (text "sstream"),
  inc <+> angles (text "limits"),
  inc <+> angles lst,
  blank,
  usingNameSpace "std" (Just "string") end,
  usingNameSpace "std" (Just $ render lst) end,
  usingNameSpace "std" (Just "ifstream") end,
  usingNameSpace "std" (Just "ofstream") end]
  where n = name m
        b = isMainMod m

cpphtop :: ModData -> Doc -> Doc -> Doc
cpphtop m lst end = vcat [
  text "#ifndef" <+> text n <> text "_h",
  text "#define" <+> text n <> text "_h",
  blank,
  inc <+> angles (text "string"),
  inc <+> angles lst,
  blank,
  usingNameSpace "std" (Just "string") end,
  usingNameSpace "std" (Just $ render lst) end,
  usingNameSpace "std" (Just "ifstream") end,
  usingNameSpace "std" (Just "ofstream") end]
  where n = name m

usingNameSpace :: Label -> Maybe Label -> Doc -> Doc
usingNameSpace n (Just m) end = text "using" <+> text n <> colon <> colon <>
  text m <> end
usingNameSpace n Nothing end = text "using namespace" <+> text n <> end

cppInherit :: Label -> Doc -> Doc
cppInherit n pub = colon <+> pub <+> text n

cppBoolTypeDoc :: TypeData
cppBoolTypeDoc = td Boolean "bool" (text "bool")

cppFloatTypeDoc :: TypeData
cppFloatTypeDoc = td Float "double" (text "double")

cppInfileTypeDoc :: TypeData
cppInfileTypeDoc = td File "ifstream" (text "ifstream")

cppOutfileTypeDoc :: TypeData
cppOutfileTypeDoc = td File "ofstream" (text "ofstream")

cppIterTypeDoc :: TypeData -> TypeData
cppIterTypeDoc t = td (Iterator (cType t)) (typeString t ++ "::iterator")
  (text "std::" <> typeDoc t <> text "::iterator")

cppClassVar :: Doc -> Doc -> Doc
cppClassVar c v = c <> text "::" <> v

cppSelfFuncApp :: (RenderSym repr) => repr (Variable repr) -> Label -> 
  repr (Type repr) -> [repr (Value repr)] -> repr (Value repr)
cppSelfFuncApp s n = funcApp (variableName s ++ "->" ++ n)

cppCast :: CppSrcCode (Type CppSrcCode) -> 
  CppSrcCode (Value CppSrcCode) -> CppSrcCode (Value CppSrcCode)
cppCast t v = cppCast' (getType t) (getType $ valueType v)
  where cppCast' Float String = funcApp "std::stod" float [v]
        cppCast' _ _ = on2CodeValues mkVal t $ on2CodeValues castObjDocD 
          (onCodeValue castDocD t) v

cppListSetDoc :: Doc -> Doc -> Doc
cppListSetDoc i v = dot <> text "at" <> parens i <+> equals <+> v

cppListDecDoc :: (RenderSym repr) => repr (Value repr) -> Doc
cppListDecDoc n = parens (valueDoc n)

cppListDecDefDoc :: (RenderSym repr) => [repr (Value repr)] -> Doc
cppListDecDefDoc vs = braces (valueList vs)

cppPrint :: Bool -> ValData -> ValData -> Doc
cppPrint newLn printFn v = valDoc printFn <+> text "<<" <+> val (valDoc v) <+> 
  end
  where val = if maybe False (< 9) (valPrec v) then parens else id
        end = if newLn then text "<<" <+> text "std::endl" else empty

cppThrowDoc :: (RenderSym repr) => repr (Value repr) -> Doc
cppThrowDoc errMsg = text "throw" <> parens (valueDoc errMsg)

cppTryCatch :: (RenderSym repr) => repr (Body repr) -> repr (Body repr) -> Doc
cppTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent $ bodyDoc tb,
  rbrace <+> text "catch" <+> parens (text "...") <+> lbrace,
  indent $ bodyDoc cb,
  rbrace]

cppDiscardInput :: (RenderSym repr) => Label -> repr (Value repr) -> Doc
cppDiscardInput sep inFn = valueDoc inFn <> dot <> text "ignore" <> parens 
  (text "std::numeric_limits<std::streamsize>::max()" <> comma <+>
  quotes (text sep))

cppInput :: VarData -> ValData -> Doc -> Doc
cppInput v inFn end = vcat [
  valDoc inFn <+> text ">>" <+> varDoc v <> end,
  valDoc inFn <> dot <> 
    text "ignore(std::numeric_limits<std::streamsize>::max(), '\\n')"]

cppOpenFile :: Label -> VarData -> ValData -> Doc
cppOpenFile mode f n = varDoc f <> dot <> text "open" <> 
  parens (valDoc n <> comma <+> text mode)

cppPointerParamDoc :: VarData -> Doc
cppPointerParamDoc v = typeDoc (varType v) <+> text "&" <> varDoc v

cppsMethod :: Label -> Label -> TypeData -> Doc -> Doc -> Doc -> Doc -> Doc
cppsMethod n c t ps b bStart bEnd = emptyIfEmpty b $ vcat [ttype <+> text c <> 
  text "::" <> text n <> parens ps <+> bStart,
  indent b,
  bEnd]
  where ttype | isDtor n = empty
              | otherwise = typeDoc t

cppsFunction :: Label -> TypeData -> Doc -> Doc -> Doc -> Doc -> Doc
cppsFunction n t ps b bStart bEnd = vcat [
  typeDoc t <+> text n <> parens ps <+> bStart,
  indent b,
  bEnd]

cpphMethod :: Label -> TypeData -> Doc -> Doc -> Doc
cpphMethod n t ps end = (if isDtor n then empty else typeDoc t) <+> text n <> 
  parens ps <> end

cppCommentedFunc :: (RenderSym repr) => FileType -> 
  MS (repr (BlockComment repr)) -> MS (repr (Method repr)) -> 
  MS (repr (Method repr))
cppCommentedFunc ft cmt fn = do
  f <- fn
  mn <- getCurrMainFunc
  scp <- getScope
  cmnt <- cmt
  let cf = toState (methodFromData scp $ commentedItem (blockCommentDoc cmnt) $ 
        methodDoc f)
      ret Source = if mn then cf else fn
      ret Header = if mn then fn else cf
      ret Combined = error "Combined passed to cppCommentedFunc"
  ret ft

cppsStateVarDef :: Label -> Doc -> BindData -> VarData -> ValData -> Doc -> Doc
cppsStateVarDef n cns p vr vl end = if bind p == Static then cns <+> typeDoc 
  (varType vr) <+> text (n ++ "::") <> varDoc vr <+> equals <+> valDoc vl <>
  end else empty

cpphStateVarDef :: (RenderSym repr) => Doc -> repr (Permanence repr) -> 
  repr (Variable repr) -> repr (Value repr) -> Doc
cpphStateVarDef s p vr vl = stateVarDocD s (permDoc p) (statementDoc $ state $ 
  if binding p == Static then varDec vr else varDecDef vr vl) 

cpphVarsFuncsList :: ScopeTag -> [StateVarData] -> [MethodData] -> Doc
cpphVarsFuncsList st vs fs = 
  let scopedVs = [stVarDoc v | v <- vs, getStVarScp v == st]
      scopedFs = [mthdDoc f | f <- fs, getMthdScp f == st]
  in vcat $ scopedVs ++ (if null scopedVs then empty else blank) : scopedFs

cppsClass :: [StateVarData] -> [MethodData] -> Doc
cppsClass vs fs = vcat $ vars ++ (if any (not . isEmpty) vars then blank else
  empty) : funcs
  where vars = map stVarDoc vs
        funcs = map mthdDoc fs

cpphClass :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> 
  Doc
cpphClass n pubs privs pub priv inhrt bStart bEnd end = vcat [
    classDec <+> text n <+> inhrt <+> bStart,
    indentList [
      pub <> colon,
      indent pubs,
      blank,
      priv <> colon,
      indent privs],
    bEnd <> end]

cpphEnum :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
cpphEnum n es bStart bEnd end = vcat [
  text "enum" <+> text n <+> bStart,
  indent es,
  bEnd <> end]

cppInOutCall :: (Label -> CppSrcCode (Type CppSrcCode) -> 
  [CppSrcCode (Value CppSrcCode)] -> CppSrcCode (Value CppSrcCode)) -> Label -> 
  [CppSrcCode (Value CppSrcCode)] -> [CppSrcCode (Variable CppSrcCode)] -> 
  [CppSrcCode (Variable CppSrcCode)] -> CppSrcCode (Statement CppSrcCode)
cppInOutCall f n ins [out] [] = assign out $ f n (variableType out) ins
cppInOutCall f n ins [] [out] = if null (filterOutObjs [out]) 
  then valState $ f n void (valueOf out : ins)
  else assign out $ f n (variableType out) (valueOf out : ins)
cppInOutCall f n ins outs both = valState $ f n void (map valueOf both ++ ins 
  ++ map valueOf outs)

cppsInOut :: (CppSrcCode (Scope CppSrcCode) -> 
    CppSrcCode (Permanence CppSrcCode) -> CppSrcCode (Type CppSrcCode) -> 
    [CppSrcCode (Parameter CppSrcCode)] -> CppSrcCode (Body CppSrcCode) -> 
    MS (CppSrcCode (Method CppSrcCode)))
  -> CppSrcCode (Scope CppSrcCode) -> CppSrcCode (Permanence CppSrcCode) -> 
  [CppSrcCode (Variable CppSrcCode)] -> [CppSrcCode (Variable CppSrcCode)] -> 
  [CppSrcCode (Variable CppSrcCode)] -> CppSrcCode (Body CppSrcCode) -> 
  MS (CppSrcCode (Method CppSrcCode))
cppsInOut f s p ins [v] [] b = f s p (variableType v) (map (onCodeValue 
  getParam) ins) (on3CodeValues surroundBody (varDec v) b (returnState $ 
  valueOf v))
cppsInOut f s p ins [] [v] b = f s p (if null (filterOutObjs [v]) then void 
  else variableType v) (map (onCodeValue getParam) $ v : ins) 
  (if null (filterOutObjs [v]) then b else on2CodeValues appendToBody b 
  (returnState $ valueOf v))
cppsInOut f s p ins outs both b = f s p void (map pointerParam both 
  ++ map (onCodeValue getParam) ins ++ map pointerParam outs) b

cpphInOut :: (CppHdrCode (Scope CppHdrCode) -> 
    CppHdrCode (Permanence CppHdrCode) -> CppHdrCode (Type CppHdrCode) -> 
    [CppHdrCode (Parameter CppHdrCode)] -> CppHdrCode (Body CppHdrCode) -> 
    MS (CppHdrCode (Method CppHdrCode))) 
  -> CppHdrCode (Scope CppHdrCode) -> CppHdrCode (Permanence CppHdrCode) -> 
  [CppHdrCode (Variable CppHdrCode)] -> [CppHdrCode (Variable CppHdrCode)] -> 
  [CppHdrCode (Variable CppHdrCode)] -> CppHdrCode (Body CppHdrCode) -> 
  MS (CppHdrCode (Method CppHdrCode))
cpphInOut f s p ins [v] [] b = f s p (variableType v) (map (onCodeValue 
  getParam) ins) b
cpphInOut f s p ins [] [v] b = f s p (if null (filterOutObjs [v]) then void 
  else variableType v) (map (onCodeValue getParam) $ v : ins) b
cpphInOut f s p ins outs both b = f s p void (map pointerParam both 
  ++ map (onCodeValue getParam) ins ++ map pointerParam outs) b