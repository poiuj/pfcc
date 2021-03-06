module Semant where

import Syntax
import Parser (parseClass)

import Data.List (foldl')

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Type =
  Type Name
  | TMethod [Type]
  | NoType -- stub, conforms with everything
  deriving (Eq)

data SemantError =
  TypeMismatch Name Name
  | CyclicInheritance [Name]
  | UndefinedClass Name
  | UndefinedVariable Name
  | UndefinedMethod Name Name
  | RedefinedAttribute Name
  | RedefinedFormal Name

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
  show (UndefinedMethod cls method) = "Method " ++ method ++ " is undefined in class " ++ cls
  show (RedefinedAttribute attr) = "Attribute is redefined: " ++ attr
  show (RedefinedFormal formal) = "Formal is redefined: " ++ formal

type Classes = M.Map Name Class
type ObjectEnv = [M.Map Name Type]
data Environment = Environment {
  envClasses :: Classes
  , envObj :: ObjectEnv
  , envCurClass :: Name
  }

type Check = ExceptT SemantError (ReaderT Environment (Writer [String]))


noClass = "NO_CLASS"
selfTypeName = "SELF_TYPE"
selfType = Type selfTypeName

parsedClass cls = case parseClass cls of
  Right classAst -> classAst
  Left errorMsg -> error $ show errorMsg

-- Parser assumes that all classes inherit from Object, so we need manual
-- specify noClass
objectClass = Class "Object" noClass [
  Method "abort" [] "Object" NoExpr
  , Method "type_name" [] "String" NoExpr
  , Method "copy" [] selfTypeName NoExpr]

ioClass = parsedClass "class IO {\
\out_string(x:String) : SELF_TYPE {self};\
\out_int(x : Int) : SELF_TYPE {self};\
\in_string() : String {\"\"};\
\in_int() : Int {0}; };"

intType = (Type "Int")
intClass = parsedClass $ "class Int {};"

stringType = (Type "String")
stringClass = parsedClass "class String {\
\length() : Int {0};\
\concat(s : String) : String {self};\
\substr(i : Int, l : Int) : String {self}; };"

boolType = (Type "Bool")
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

makeFormalType :: Formal -> Type
makeFormalType = Type . formalType

makeMethodType :: [Formal] -> Name -> Type
makeMethodType formals result =
  TMethod $ (map makeFormalType formals) ++ [(Type result)]

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

lookupMethod :: Type -> Name -> Check Type
lookupMethod (Type className) methodName = lookupMethodInner className
  where lookupMethodInner currentClassName
          | currentClassName == noClass =
            throwError $ UndefinedMethod className methodName
          | otherwise = do
          cls <- lookupClass currentClassName
          case lookupMethodInFeatures (classFeatures cls) methodName of
            Just t -> return t
            Nothing -> lookupMethodInner $ classBase cls

lookupClass :: Name -> Check Class
lookupClass name = do
  lookupResult <- reader $ M.lookup name . envClasses
  case lookupResult of
    Just cls -> return cls
    Nothing -> throwError $ UndefinedClass name

updateObjEnv :: ObjectEnv -> Environment -> Environment
updateObjEnv newEnvObj env = env { envObj = newEnvObj }

-- Monoid?
emptyObjEnv = [M.singleton "self" selfType]
mergeEnvs = (++)
mapToObjEnv = (:[])

makeLocalInner :: [Feature] -> M.Map Name Type -> Check (M.Map Name Type)
makeLocalInner ((Method _ _ _ _):fs) map = makeLocalInner fs map
makeLocalInner [] map = return map
makeLocalInner ((Attribute aName aType aInit):fs) map = do
  -- do not use lookupVariable here because we don't want to throw an error
  -- in case of failure
  previouslyDefinedAttr <- reader $ lookupInObjectEnv aName . envObj
  case previouslyDefinedAttr of
    Just t -> throwError $ RedefinedAttribute aName
    Nothing -> do
      let resultType = Type aType
      initType <- checkExpr aInit
      resultType `conforms` initType
      makeLocalInner fs (M.insert aName resultType map)

makeLocalObjEnv :: [Feature] -> Check ObjectEnv
makeLocalObjEnv features = do
  attrsMap <- makeLocalInner features M.empty
  return $ mapToObjEnv attrsMap

addToObjEnv :: Name -> Type -> Check ObjectEnv
addToObjEnv x t = do
  (e:es) <- reader $ envObj
  return $ (M.insert x t e) : es

makeObjEnvFromFormals :: [Formal] -> Check ObjectEnv
makeObjEnvFromFormals = makeObjEnvFromFormals' M.empty
  where
    makeObjEnvFromFormals' :: M.Map Name Type -> [Formal] -> Check ObjectEnv
    makeObjEnvFromFormals' map ((Formal n t):fs) =
      case M.lookup n map of
        Just t -> throwError $ RedefinedFormal n
        Nothing -> makeObjEnvFromFormals' (M.insert n (Type t) map) fs
    makeObjEnvFromFormals' map _ = return $ mapToObjEnv map

makeObjEnvForClass :: Class -> Check ObjectEnv
makeObjEnvForClass (Class "Object" _ _) = return emptyObjEnv
makeObjEnvForClass (Class name base features) = do
  baseClass <- lookupClass base
  baseObjEnv <- makeObjEnvForClass baseClass
  localObjEnv <- local (updateObjEnv baseObjEnv) (makeLocalObjEnv features)
  return $ mergeEnvs localObjEnv baseObjEnv

throwTypeMismatch :: Type -> Type -> Check ()
throwTypeMismatch (Type name1) (Type name2) =
  throwError $ TypeMismatch name1 name2
throwTypeMismatch (Type tName) (TMethod _) =
  throwError $ TypeMismatch tName "method"
throwTypeMismatch m@(TMethod _) t@(Type _) = throwTypeMismatch t m

-- T1 `conforms` T2 === T1 <= T2
conforms :: Type -> Type -> Check ()
conforms (Type name1) (Type name2) = conformsInner name1
  where
    conformsInner n1
      | n1 == name2 = return ()
      | n1 == selfTypeName = reader envCurClass >>= conformsInner
      | n1 == noClass = throwError $ TypeMismatch name1 name2
      | otherwise = do
          (Class _ base _) <- lookupClass n1
          conformsInner base
conforms NoType t2 = return ()
conforms t1 NoType = NoType `conforms` t1
conforms t1 t2 = throwTypeMismatch t1 t2

isType :: Type -> Type -> Check ()
isType actualType expectedType =
  if actualType == expectedType
    then return ()
    else throwTypeMismatch actualType expectedType

hasType :: Expr -> Type -> Check ()
hasType expr expectedType = do
  exprType <- checkExpr expr
  exprType `isType` expectedType

isPrimitiveType :: Type -> Bool
isPrimitiveType = (`elem` [intType, stringType, boolType])

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
checkClass cls@(Class name _ features) = do
  clsObjEnv <- makeObjEnvForClass cls
  local (updateEnv clsObjEnv name) $ forM_ features checkMethod
  where updateEnv obj cls env =
          env { envObj = obj, envCurClass = cls }

checkMethod :: Feature -> Check ()
checkMethod (Attribute _ _ _) = return ()
checkMethod (Method name formals result body) = do
  methodObjEnv <- makeObjEnvFromFormals formals
  bodyType <- local (updateObjEnv methodObjEnv) (checkExpr body)
  bodyType `conforms` (Type result)

checkArith :: Expr -> Expr -> Check Type
checkArith e1 e2 =
  e1 `hasType` intType >> e2 `hasType` intType >> return intType

checkCompare :: Expr -> Expr -> Check Type
checkCompare e1 e2 =
  e1 `hasType` intType >> e2 `hasType` intType >> return boolType


checkExpr :: Expr -> Check Type
checkExpr NoExpr = return NoType

checkExpr (Int _) = return intType
checkExpr (StringConst _) = return stringType
checkExpr (BoolConst _) = return boolType

checkExpr (Id name) = lookupVariable name

checkExpr (Assignment lhs rhs) = do
  lhsType <- lookupVariable lhs
  rhsType <- checkExpr rhs
  rhsType `conforms` lhsType
  return rhsType

checkExpr (New typeName) = return $ Type typeName

checkExpr (Compound exprs) = foldl' (\_ -> checkExpr) (return NoType) exprs

checkExpr (Let x tname initExpr body) = do
  initType <- checkExpr initExpr
  let xType = (Type tname)
  initType `conforms` xType
  objEnv' <- addToObjEnv x xType
  local (updateObjEnv objEnv') (checkExpr body)

checkExpr (UnExpr Not expr) = expr `hasType` boolType >> return boolType
checkExpr (UnExpr Complement expr) = expr `hasType` intType >> return intType
checkExpr (UnExpr IsVoid expr) = return boolType

checkExpr (BinExpr Mul e1 e2) = checkArith e1 e2
checkExpr (BinExpr Div e1 e2) = checkArith e1 e2
checkExpr (BinExpr Plus e1 e2) = checkArith e1 e2
checkExpr (BinExpr Minus e1 e2) = checkArith e1 e2

checkExpr (BinExpr Le e1 e2) = checkCompare e1 e2
checkExpr (BinExpr Lt e1 e2) = checkCompare e1 e2

checkExpr (BinExpr Eq e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  if (isPrimitiveType t1 || isPrimitiveType t2)
    then t1 `isType` t2 >> return boolType
    else return boolType

checkExpr (Call expr name args) = do
  exprType <- checkExpr expr
  actualTypes <- forM args checkExpr

  exprType' <- if (exprType == selfType)
                 then reader $ Type . envCurClass
                 else return exprType

  (TMethod formalTypes) <- lookupMethod exprType' name

  zipWithM_ conforms (init actualTypes) (init formalTypes)

  let returnType' = last formalTypes
  if returnType' == selfType
    then return exprType
    else return returnType'

checkExpr (StaticCall expr className methodName args) = do
  exprType <- checkExpr expr
  actualTypes <- forM args checkExpr

  let classType = (Type className)
  exprType `conforms` classType

  (TMethod formalTypes) <- lookupMethod classType methodName

  zipWithM_ conforms (init actualTypes) (init formalTypes)

  let returnType' = last formalTypes
  if returnType' == selfType
    then return exprType
    else return returnType'

checkExpr _ = undefined


semantDriver :: Program -> Check ()
semantDriver program = checkInheritance program >> checkProgram program

semant :: Program -> Either SemantError ()
semant program = fst $ runWriter (runReaderT (runExceptT $ semantDriver program) (Environment (classesMap program) [] noClass))
