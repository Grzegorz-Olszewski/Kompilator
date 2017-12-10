{-# LANGUAGE Safe #-}

module GrzegorzOlszewski (typecheck, eval) where
import AST
import DataTypes
type Env a = [(Var,a)]
type EnvFunc a = [(FSym, a)]
data Val = VInt Integer | VBool Bool | VUnit | VPair Val Val | VList [Val]  deriving (Eq)    
type Error p = String 
data RunErr = RunErr
makeTIntPair :: Var -> (Var,Type)
makeTIntPair x = (x, TInt)

env_type_init :: [Var] -> [(Var,Type)]
env_type_init [] = []
env_type_init (x:xs) = map makeTIntPair (x:xs)

check_funcs2 :: [FunctionDef p] -> [(FSym,(Type,Type))] ->Maybe (Error p)
check_funcs2 [] _ = Nothing
check_funcs2 ((FunctionDef p name arg typeIn typeOut body):xs) func = case (infer func g body, check_funcs2 xs func) of
    (Right typeOut, Nothing ) -> Nothing
    _ -> Just "Function types are not congruent"
    where g = if typeIn==TUnit then [] else [(arg,typeIn)]

makeVIntPair :: (Var,Integer) -> (Var, Val)
makeVIntPair (var, int)= (var , (VInt int))

env_val_init :: [(Var,Integer)] -> [(Var,Val)]
env_val_init [] = []
env_val_init (x:xs) = map makeVIntPair (x:xs)

makeFuncTrioType :: FunctionDef p-> (FSym, (Type, Type))
makeFuncTrioType (FunctionDef p name arg typeIn typeOut body) = (name,(typeIn,typeOut))

funcenv_type_init :: [FunctionDef p] -> [(FSym,(Type,Type))]
funcenv_type_init [] = []
funcenv_type_init xs = map makeFuncTrioType xs

makeFuncTrioValue :: FunctionDef p -> (FSym, (Var, Expr p))
makeFuncTrioValue (FunctionDef p name arg typeIn typeOut body) = (name, (arg, body))

funcenv_value_init :: [FunctionDef p] -> [(FSym, (Var, Expr p))]
funcenv_value_init [] = [] 
funcenv_value_init xs = map makeFuncTrioValue xs

typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck funcs vars ex =
    if check_funcs2 funcs funcEnv  == Nothing then
        case res of
            Left err -> Error (getData ex) err
            Right TInt -> Ok
            otherwise -> Error (getData ex) "The program returns different type than int " 
    else Error (getData ex) "Function types are not congruent"
                where
                    env = env_type_init vars
                    funcEnv = funcenv_type_init funcs
                    res = infer funcEnv env ex
    
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval funcs xs ex = 
    case res of
        Left RunErr ->  RuntimeError
        Right (VInt val) -> (Value val)
        where 
            funcEnv = funcenv_value_init funcs
            env = env_val_init xs
            res = evaluate funcEnv env ex

check_if_arith_bin_op_type :: BinaryOperator -> Int

check_if_arith_bin_op_type op 
    -- return 1 if op is arithmetic operator
    | elem op [BAdd, BSub, BMul, BDiv, BMod] = 1
    -- return 2 if op is an operator that compares numbers
    | elem op [BEq, BNeq, BLt, BGt, BLe, BGe] = 2
    -- return 3 if op is logic operator
    | elem op [BAnd, BOr] = 3


infer :: EnvFunc (Type,Type) -> Env Type -> Expr p -> Either (Error p) Type

infer funcs env (ENum _ _) = Right TInt
infer funcs env (EBool _ _) = Right TBool
infer funcs env (EUnit _) = Right TUnit
infer funcs env (EPair _ ex1 ex2) = 
    case (infer funcs env ex1, infer funcs env ex2) of
        (Left err, _) -> Left err
        (_ , Left err) -> Left err
        (Right typ1, Right typ2) -> Right (TPair typ1 typ2)

infer funcs env (EFst _ e) = 
    case infer funcs env e of
        Left err -> Left err
        Right (TPair t _ ) -> Right t
        Right t2 -> Left $ "Exprected a pair got" ++ show t2

infer funcs env (ESnd _ e) = 
    case infer funcs env e of
        Left err -> Left err
        Right (TPair _ t ) -> Right t
        Right t2 -> Left $ "Exprected a pair got" ++ show t2

infer funcs env (ENil _ type1) =
    case type1 of
         (TList _)  -> Right type1
         _ -> Left $ "Expected a list got " ++ show type1
infer funcs env (ECons _ ex1 ex2) = 
    case infer funcs env ex1 of
        Left err  -> Left err
        Right t -> case infer funcs env ex2 of
            Left err -> Left err
            Right (TList t2) -> if t==t2 then Right (TList t) else Left $ "Expected tail of the same type as head, got " ++ show t2
            Right t3 -> Left $ "Expected tail of the same type as head, got " ++ show t3

infer funcs env (EApp _ name ex1 ) =
    case infer funcs env ex1 of 
        Left err -> Left err
        Right type1 -> case lookup name funcs of
            Nothing -> Left "Function with that name doesnt exist"
            Just x -> if fst x == type1 then Right (snd x) else Left "Argument type is different than input type."

        
infer funcs env (EMatchL _ ex1 ex2  (var1 ,var2, ex3  )) =
    case infer funcs env ex1 of 
        Left err -> Left err
        Right (TList t) -> case infer funcs env ex2 of
            Left err -> Left err
            Right t1 -> case infer funcs env2 ex3 of
                Left err -> Left err
                Right t2 -> if t1 == t2 then Right t1 else Left "Alternatives should have the same type." 
                where 
                    env1 = extendEnv var1 t env 
                    env2 = extendEnv var2 (TList t) env1
        
infer funcs env (EUnary _ UNeg e) =
    case infer funcs env e of 
        Left err -> Left err
        Right TInt -> Right TInt
        otherwise -> Left "Expression should have int type, has something different."
infer funcs env (EUnary _ UNot e) =
    case infer funcs env e of 
        Left err -> Left err
        Right TBool -> Right TBool
        otherwise -> Left "Expression should have bool type, has something different."


infer funcs env (EBinary _ op e1 e2) =
    case check_if_arith_bin_op_type op of
        1 -> case infer funcs env e1 of
            Left err -> Left err
            Right TInt -> case infer funcs env e2 of
                Left err -> Left err
                Right TInt -> Right TInt
                otherwise-> Left "Expression should have int type, has something different."
            otherwise -> Left "Expression should have int type, has something different."
        2 -> case infer funcs env e1 of
            Left err -> Left err
            Right TInt  -> case infer funcs env e2 of
                Left err -> Left err
                Right TInt -> Right TBool
                otherwise  -> Left "Expression should have int type, has something different."
            otherwise  -> Left "Expression should have int type, has something different."
        3 -> case infer funcs env e1 of
            Left err -> Left err
            Right TBool -> case infer funcs env e2 of
                Left err -> Left err
                Right TBool -> Right TBool
                otherwise-> Left  "Expression should have bool type, has something different."
            otherwise  ->  Left  "Expression should have bool type, has something different."
infer funcs env (EIf _ e1 e2 e3) =
    case infer funcs env e1 of
        Left err -> Left err
        Right TBool -> case infer funcs env e2 of
            Left err -> Left err
            Right t -> case infer funcs env e3 of
                Left err -> Left err
                Right t2 -> if t==t2 then Right t else Left "Different types."
        otherwise -> Left  "Expression should have bool type, has something different."
infer funcs gamma (EVar _ x) = 
    case lookup x gamma of
        Just y -> Right y
        Nothing -> Left "This variable doesnt exist in the environment."
infer funcs gamma (ELet _ var e1 e2) =
    case infer funcs gamma e1 of
        Left err -> Left err
        Right typ1 -> case infer funcs gamma1 e2 of
            Left err -> Left err
            Right typ2 -> Right typ2
            where
                gamma1 = extendEnv var typ1 gamma
        

extendEnv :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
extendEnv key value [] = [(key,value)]
extendEnv key value assoc = (key,value):(filter ((key /=).fst) assoc)

check_if_arith_bin_op_val :: BinaryOperator -> Integer 
check_if_arith_bin_op_val op 
    -- return 1 if op is arithmetic operator
    | elem op [BAdd, BSub, BMul] = 1
    -- return 2 if op is arithmetic operator and second argument shouldn't be 0
    | elem op [BDiv,BMod] = 2
    -- return 3 if op is an operator that compares numbers
    | elem op [BEq, BNeq, BLt, BGt, BLe, BGe] = 3
    -- return 4 if op is logic operator
    | elem op [BAnd, BOr] = 4

get_op op (x:xs) = y where
    Just y = lookup op (x:xs)

evaluate :: [(FSym, (Var, Expr p))] ->Env Val -> Expr p -> Either RunErr Val 

evaluate funcs env (ENum _ x) =  Right (VInt x)
evaluate funcs env (EBool _ x) =  Right (VBool x)
evaluate funcs env (EUnary p UNeg x) = Right (VInt (-e))
    where Right (VInt e) = evaluate funcs env x
evaluate funcs env (EPair p ex1 ex2) = 
    case (evaluate funcs env ex1, evaluate funcs env ex2) of
        (Left RunErr, _) -> Left RunErr
        (_, Left RunErr) -> Left RunErr
        (Right x1, Right x2) -> Right (VPair x1 x2)
evaluate funcs env (EUnit p) = Right VUnit
evaluate funcs env (EApp p ff expr) = case lookup ff funcs of
    Just (var,body) -> case evaluate funcs env expr of
        Left RunErr -> Left RunErr
        Right something -> case evaluate funcs [(var,something)] body of
            Left RunErr -> Left RunErr
            Right some -> Right some
evaluate funcs env (ENil p _) = Right (VList [])
evaluate funcs env (ECons p ex1 ex2) = 
    case evaluate funcs env ex1 of
        Left RunErr -> Left RunErr
        Right head1 -> case evaluate funcs env ex2 of
            Left err -> Left err
            Right (VList tail1)-> Right (VList (head1:tail1)) where

evaluate funcs env (EFst p ex) = case evaluate funcs env ex of
    Left RunErr -> Left RunErr
    Right (VPair ex1 ex2) -> Right ex1
evaluate funcs env (ESnd p ex) = case evaluate funcs env ex of
    Left RunErr -> Left RunErr
    Right (VPair ex1 ex2) -> Right ex2
evaluate funcs env (EUnary p UNot x) =
    case evaluate funcs env x of
        Right (VBool True) -> Right (VBool False)
        Right (VBool False) -> Right (VBool True)

evaluate funcs env (EBinary p op ex1 ex2) = 
    case (check_if_arith_bin_op_val op) of
        1 -> case (evaluate funcs env ex1, evaluate funcs env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (Right (VInt x1), Right (VInt x2)) -> Right (VInt (func x1 x2)) where
                func = get_op op [(BAdd, (+)), (BSub,(-)), (BMul,(*))]
        2 -> case (evaluate funcs env ex1, evaluate funcs env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (_, Right (VInt 0)) -> Left RunErr
            (Right (VInt x1), Right (VInt x2)) -> Right (VInt (func x1 x2)) where
                func = get_op op [(BDiv, div), (BMod, mod)]
        3 -> case (evaluate funcs env ex1, evaluate funcs env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (Right (VInt x1), Right (VInt x2)) -> Right (VBool (func x1 x2)) where
                func = get_op op [(BEq, (==)), (BNeq, (/=)), (BLt, (<)), (BGt, (>)), (BLe, (<=)), (BGe, (>=))]
        4 -> case (evaluate funcs env ex1, evaluate funcs env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (Right (VBool x1), Right (VBool x2)) -> Right (VBool (func x1 x2)) where
                func = get_op op [(BAnd, (&&)), (BOr,(||))]  
                
evaluate funcs env (EIf p ex1 ex2 ex3) = 
    case evaluate funcs env ex1 of
        Left RunErr -> Left RunErr
        Right (VBool True) -> case evaluate funcs env ex2 of
            Left RunErr-> Left RunErr
            Right t -> Right t 
        Right (VBool False) -> case evaluate funcs env ex3 of
            Left RunErr -> Left RunErr
            Right t -> Right t

evaluate funcs env (EVar _ x) = 
    case lookup x env of
        Just y -> Right y
        Nothing -> Left RunErr

evaluate funcs env (ELet _ var e1 e2) = 
    case evaluate funcs env e1 of
        Left RunErr -> Left RunErr
        Right t -> case evaluate funcs env2 e2 of
            Left RunErr -> Left RunErr
            Right expr -> Right expr
            where env2 = extendEnv var t env

evaluate funcs env (EMatchL _ ex1 ex2 (var1, var2, ex3) ) =
    case evaluate funcs env ex1 of
        Left err -> Left err
        Right (VList t) -> case t of 
            [] ->  evaluate funcs env ex2
            (x:xs) -> evaluate funcs env2 ex3 where
                env1 = extendEnv var1  x env
                env2 = extendEnv var2 (VList xs) env1 

