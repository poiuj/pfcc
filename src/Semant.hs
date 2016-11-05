module Semant where

import Syntax

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
  | Method [Name]


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

classesMap :: Program -> Classes
classesMap = M.fromList . (map (\cls -> (className cls, cls))) . programClasses

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
