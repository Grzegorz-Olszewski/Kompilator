{-# LANGUAGE Safe #-}

module GrzegorzOlszewskiCompiler(compile) where
import Control.Monad.State.Strict 
import AST
import MacroAsm
import System.Random

-- ROZWIĄZANIE DLA PRACOWNI NR 4

number_elements :: [Var] -> Integer-> [(Var,Integer)]
number_elements [] count = []
number_elements (x:xs) count = (x,count): number_elements xs (count+1)

compile :: [FunctionDef p] -> [Var] -> Expr p -> [MInstr]
compile funcs vars expr = 
    (fst res) ++ [MRet] where
        env = number_elements vars 0
        res = execute env expr 0 

add_one_to_env :: [(Var,Integer)] -> [(Var,Integer)]

add_one_to_env env = map (\(a,b) -> (a,b+1)) env

check_if_arith_bin_op_type :: BinaryOperator -> Int

check_if_arith_bin_op_type op 
    | elem op [BAdd, BSub, BMul, BDiv, BMod,BAnd, BOr] = 1
    | elem op [BEq, BNeq, BLt, BGt, BLe, BGe] = 2
    
execute :: [(Var,Integer)] -> Expr p -> Int -> ([MInstr], Int)

execute env (ENum _ n) k = ([MConst n], k)

execute env (EBool _ True) k = ([MConst (-1)], k)

execute env (EBool _ False) k = ([MConst 0], k)

execute env (EUnary _ UNot ex1) k = case execute env ex1 k of  
    ([MConst (-1)], k1) -> ([MConst 0], k1)
    ([MConst 0],k2 ) -> ([MConst (-1)],k2)


execute env (EUnary _ UNeg ex1) k = ([MConst (-n)], k1) where ([MConst n], k1) = execute env ex1 k


execute env (EBinary _ op ex1 ex2) k = 
    case check_if_arith_bin_op_type op of
        1 -> (exp1 ++ [MPush] ++ exp2 ++ [op1], k2) where   
                (exp1, k1) = execute env ex1 k
                env1 = add_one_to_env env
                (exp2, k2) = execute env1 ex2 k1
                Just op1 = lookup op [(BAdd, MAdd), (BSub, MSub), (BMul, MMul), (BDiv, MDiv), (BMod, MMod), (BOr, MOr), (BAnd, MAnd)]
        2 -> (exp1 ++ [MPush] ++ exp2 ++ [MBranch op1 et1] ++ [MConst 0]++ [MJump et2] ++ [MLabel et1] ++ [MConst (-1)] ++ [MLabel et2], k3) where 
                (Just op1) = lookup op [(BEq, MC_EQ), (BNeq, MC_NE),(BLt, MC_LT), (BGt, MC_GT), (BLe, MC_LE), (BGe, MC_GE)]
                et1 = k
                et2 = k+1
                k1 = k+2
                (exp1, k2) = execute env ex1 k1
                env1 = add_one_to_env env
                (exp2, k3) = execute env1 ex2 k2

execute env (EVar _ var) k = ([MGetLocal res1], k) where
    res1 = fromIntegral res
    Just res = lookup var env

execute env (ELet _ var e1 e2) k = 
    (exp1 ++ [MPush] ++ exp2, k2)
    where 
        (exp1, k1) = execute env e1 k
        env1 = add_one_to_env env 
        env2 = [(var,0)] ++ env1
        (exp2, k2) = execute env2 e2 k1


execute env (EIf _ e1 e2 e3) k =
    (exp1 ++ [MBranch MC_Z et1] ++ exp2 ++ [MJump et2] ++ [MLabel et1] ++ exp3 ++ [MLabel et2], k4)
    where
        et1 = k
        et2 = k+1
        k1 = k+2
        (exp1, k2) = execute env e1 k1
        (exp2, k3) = execute env e2 k2
        (exp3, k4) = execute env e3 k3
        


