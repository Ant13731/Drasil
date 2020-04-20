module Test.Helper (helper) where

import GOOL.Drasil (SFile, SMethod,
  OOProg, FileSym(..), PermanenceSym(..), bodyStatements, TypeSym(..), 
  DeclStatement(..), ControlStatement(..), (&=), VariableSym(..),
  Literal(..), VariableValue(..), NumericExpression(..), ScopeSym(..), ParameterSym(..), MethodSym(..), 
  ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (OOProg r) => SFile r
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd] [])

doubleAndAdd :: (OOProg r) => SMethod r
doubleAndAdd = docFunc "This function adds two numbers" 
  ["First number to add", "Second number to add"] (Just "Sum") $ 
  function "doubleAndAdd"  public static double
  [param $ var "num1" double, param $ var "num2" double]
  (bodyStatements [
    varDec $ var "doubledSum" double, 
    var "doubledSum" double &= ((litDouble 2.0 #* valueOf (var "num1" double)) #+ 
      (litDouble 2.0 #* valueOf (var "num2" double))),
    returnStmt (valueOf (var "doubledSum" double))])