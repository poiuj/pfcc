module Semant where

import Syntax
import Parser (parseClass)

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Type =
  TBool
  | TInt
  | TString
  | TObject
  | Type Name
  | TMethod [Name]


data SemantError =
  TypeMismatch Name Name
  | CyclicInheritance [Name]
  | UndefinedClass Name

showCyclicClasses :: [Name] -> String
showCyclicClasses = foldr step ""
  where
    step c "" = showClass c
    step c msg = showClass c ++ ", " ++ msg

showClass :: Name -> String
showClass = id

instance Show SemantError where
  show (TypeMismatch t1 t2) = "Expected type: " ++ show t1 ++ ", actual type: " ++ show t2
  show (CyclicInheritance classes) = "Cyclic inheritance: " ++ showCyclicClasses classes
  show (UndefinedClass cls) = "Class is undefined: " ++ showClass cls


type Classes = M.Map Name Class

type Check = ExceptT SemantError (Reader Classes)


parsedClass cls = case parseClass cls of
  Right classAst -> classAst
  Left errorMsg -> error $ show errorMsg

-- Parser assumes that all classes inherit from Object, so we need manual
-- specify "NO_CLASS"
objectClass = Class "Object" "NO_CLASS" [
  Method "abort" [] "Object" NoExpr
  , Method "type_name" [] "String" NoExpr
  , Method "copy" [] "SELF_TYPE" NoExpr]

ioClass = parsedClass "class IO {\
\out_string(x:String) : SELF_TYPE {{}};\
\out_int(x : Int) : SELF_TYPE {{}};\
\in_string() : String {{}};\
\in_int() : Int {{}}; };"

intClass = parsedClass "class Int {};"

stringClass = parsedClass "class String {\
\length() : Int {{}};\
\concat(s : String) : String {{}};\
\substr(i : Int, l : Int) : String {{}}; };"

boolClass = parsedClass "class Bool {};"

basicClasses :: [Class]
basicClasses = [objectClass, ioClass, intClass, stringClass, boolClass]

classesMap :: Program -> Classes
classesMap = M.fromList . (map (\cls -> (className cls, cls))) . includeBasicClasses . programClasses
  where includeBasicClasses = (basicClasses ++)

checkInheritance :: Program -> Check ()
checkInheritance program = (mapM checkClass $ programClasses program) >> return ()

checkClass :: Class -> Check ()
checkClass = innerCheck S.empty
  where innerCheck :: S.Set Name -> Class -> Check ()
          -- TODO: support of std lib
        innerCheck _ (Class _ "Object" _) = return ()
        innerCheck seen (Class name _ _) | name `S.member` seen = throwError $ CyclicInheritance (S.toList seen)
        innerCheck seen (Class name base _) = do
          baseClassLookup <- reader $ M.lookup base
          case baseClassLookup of
            (Just baseClass) -> innerCheck (S.insert name seen) baseClass
            Nothing -> throwError $ UndefinedClass base

semant :: Program -> Either SemantError ()
semant program = runReader (runExceptT $ checkInheritance program) (classesMap program)
