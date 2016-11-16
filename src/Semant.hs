module Semant where

import Syntax
import Parser (parseClass)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Type =
  Type Name
  | TMethod [Name]


data SemantError =
  TypeMismatch Name Name
  | CyclicInheritance [Name]
  | UndefinedClass Name
  | UndefinedVariable Name
  | UndefinedMethod Name
  | RedefinedAttribute Name

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
  show (UndefinedVariable var) = "Variable is undefined: " ++ var
  show (UndefinedMethod method) = "Method is undefined: " ++ method
  show (RedefinedAttribute attr) = "Attribute is redefined: " ++ attr

type Classes = M.Map Name Class
type ObjectEnv = [M.Map Name Type]
data Environment = Environment {
  envClasses :: Classes
  , envObj :: ObjectEnv
  }

type Check = ExceptT SemantError (ReaderT Environment (Writer [String]))


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

lookupInObjectEnv :: Name -> ObjectEnv -> Maybe Type
lookupInObjectEnv name (env:envs) =
  case M.lookup name env of
    Just t -> Just t
    Nothing -> lookupInObjectEnv name envs
lookupInObjectEnv _ _ = Nothing

lookupVariable :: Name -> Check Type
lookupVariable name = do
  env <- reader envObj
  case lookupInObjectEnv name env of
    Just t -> return t
    Nothing -> throwError $ UndefinedVariable name

makeMethodType :: [Formal] -> Name -> Type
makeMethodType formals result = TMethod $ (map formalType formals) ++ [result]

lookupMethodInFeatures :: [Feature] -> Name -> Maybe Type
lookupMethodInFeatures (method:features) name =
  case method of
    Method mName mFormals mResult _ ->
      if mName == name
      then Just $ makeMethodType mFormals mResult
      else lookupMethodInFeatures features name
    -- if it's not method, ignore it
    _ -> lookupMethodInFeatures features name
lookupMethodInFeatures _ _ = Nothing

lookupMethod :: Name -> Name -> Check Type
lookupMethod className methodName = do
  classes <- reader envClasses
  case M.lookup className classes of
    Just (Class name base features) -> do
      case lookupMethodInFeatures features methodName of
        Just t -> return t
        Nothing -> lookupMethod base methodName
    Nothing -> throwError $ UndefinedMethod methodName

lookupClass :: Name -> Check Class
lookupClass name = do
  lookupResult <- reader $ M.lookup name . envClasses
  case lookupResult of
    Just cls -> return cls
    Nothing -> throwError $ UndefinedClass name

updateObjEnv :: Environment -> ObjectEnv -> Environment
updateObjEnv (Environment classes _) newObjEnv = Environment classes newObjEnv

-- Monoid?
emptyObjEnv = []
mergeEnvs = (++)
fromMap = (:[])

makeLocalInner :: [Feature] -> M.Map Name Type -> Check (M.Map Name Type)
makeLocalInner ((Method _ _ _ _):fs) map = makeLocalInner fs map
makeLocalInner [] map = return map
makeLocalInner ((Attribute aName aType aInit):fs) map = do
  previouslyDefinedAttr <- reader $ lookupInObjectEnv aName . envObj
  case previouslyDefinedAttr of
    Just t -> throwError $ RedefinedAttribute aName
    Nothing -> makeLocalInner fs (M.insert aName (Type aType) map)

makeLocalObjEnv :: [Feature] -> Check ObjectEnv
makeLocalObjEnv features = do
  attrsMap <- makeLocalInner features M.empty
  return $ fromMap attrsMap

makeObjEnvForClass :: Class -> Check ObjectEnv
makeObjEnvForClass (Class "Object" _ _) = return emptyObjEnv
makeObjEnvForClass (Class name base features) = do
  baseClass <- lookupClass base
  baseObjEnv <- makeObjEnvForClass baseClass
  localObjEnv <- local (flip updateObjEnv baseObjEnv) (makeLocalObjEnv features)
  return $ mergeEnvs localObjEnv baseObjEnv

checkInheritance :: Program -> Check ()
checkInheritance = mapM_ checkClassInheritance . programClasses

checkClassInheritance :: Class -> Check ()
checkClassInheritance = innerCheck S.empty
  where innerCheck :: S.Set Name -> Class -> Check ()
          -- TODO: support of std lib
        innerCheck _ (Class _ "Object" _) = return ()
        innerCheck seen (Class name _ _) | name `S.member` seen = throwError $ CyclicInheritance (S.toList seen)
        innerCheck seen (Class name base _) = do
          baseClass <- lookupClass base
          innerCheck (S.insert name seen) baseClass

checkProgram :: Program -> Check ()
checkProgram = mapM_ checkClass . programClasses

checkClass :: Class -> Check ()
checkClass cls = do
  -- clsObjEnv <- makeObjEnvForClass cls
  -- local (flip updateObjEnv clsObjEnv) ask
  return ()

semant :: Program -> Either SemantError ()
semant program = fst $ runWriter (runReaderT (runExceptT $ checkInheritance program) (Environment (classesMap program) []))
