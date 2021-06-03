{-# OPTIONS_GHC -Wall #-}
module Semantics where 

import Syntax 
import Control.Monad.Trans.State.Lazy
import Control.Monad

getValue:: String -> Work -> Integer
getValue s = \(_,mem,_) -> case lookup s mem of
                              Just v  -> v 
                              Nothing -> error "getValue"

updValue :: String -> Integer -> Work -> Work
updValue s v = \(inp,mem,out) -> (inp, update mem s v, out)
  where update :: [(String, Integer)] -> String -> Integer ->  [(String, Integer)]
        update [] _ _           = error "updValue" 
        update ((x,_):sx) s1 v1 | x==s1 = (x,v1): sx
        update (x:sx) s1 v1     = x:(update sx s1 v1) 

writeValue :: Integer -> Work -> Work 
writeValue v = \(inp, mem, out) -> (inp, mem, out ++ [v])

extMemory :: [String] -> Work -> Work 
extMemory vs = \(inp,mem,out) -> (inp,[(v,0)|v<-vs]++mem,out)

dropMemory :: Int -> Work -> Work 
dropMemory t = \(inp,mem,out) -> (inp, drop t mem,out)

readInput :: Work -> Integer
readInput = \(inp,_,_) -> if null inp then error "readInput" else head inp 

dropInput :: Work -> Work 
dropInput = \(inp,mem,out) -> (tail inp,mem,out)

applyBo :: Op -> Integer -> Integer -> Integer 
applyBo Plus v1 v2  = v1 + v2  
applyBo Minus v1 v2 = v1 - v2
applyBo Times v1 v2 = v1 * v2
applyBo Div v1 v2   = if v2 /= 0 then div v1 v2 else error "DivOnZero"
applyBo Mod v1 v2   = if v2 /= 0 then mod v1 v2 else error "ModOnZero" 

-------------------------------------
eExpr :: Expr -> Work -> Integer
eExpr (Var s)          = \w -> getValue s w 
eExpr (Const v)        = \_ -> v 
eExpr (BinOp op e1 e2) = \w -> applyBo op (eExpr e1 w) (eExpr e2 w) 

iStmt :: Stmt -> Work -> Work 
iStmt (Assign var e) = \w -> updValue var (eExpr e w) w 
iStmt (If e s)       = \w -> if eExpr e w > 0 then iStmt s w else w
iStmt wh@(While e s) = \w -> if eExpr e w > 0 then iStmt wh (iStmt s w) else w  
iStmt (Block vs sts) = \w -> let w1 = extMemory vs w
                                 w2 = foldl (flip iStmt) w1 sts 
                             in dropMemory (length vs) w2
iStmt (Read var)     = \w -> let v = readInput w
                             in updValue var v (dropInput w)
iStmt (Write e)      = \w -> writeValue (eExpr e w) w

iProgram :: Program -> [Integer] -> [Integer]
iProgram prog ix = let w = (ix, [],[])          
                       (_,_,ox) = iStmt prog w  
                   in ox

--------------------- State -----------------
eExprS :: Expr -> State Work Integer
eExprS (Var s)          = do w <- get 
                             return $ getValue s w 
eExprS (Const v)        = return v 
eExprS (BinOp op e1 e2) = do v1 <- eExprS e1 
                             v2 <- eExprS e2
                             return $ applyBo op v1 v2  

iStmtS :: Stmt -> State Work () 
iStmtS (Assign var e) = do v <- eExprS e
                           modify (updValue var v) 
iStmtS (If e s)       = do v <- eExprS e 
                           if v > 0 then iStmtS s else return ()
                           -- when (v>0) (iStmtS s)
iStmtS wh@(While e s) = do v <- eExprS e
                           if v > 0 then do iStmtS s 
                                            iStmtS wh  
                                    else return ()
                           -- when (v>0) (iStmtS s >> iStmtS wh)
iStmtS (Block vs sts) = do modify (extMemory vs)
                           mapM_ iStmtS sts   
                           modify (dropMemory (length vs))
iStmtS (Read var)     = do w <- get 
                           let v = readInput w
                           modify (updValue var v)
                           modify dropInput
iStmtS (Write e)      = do v <- eExprS e
                           modify (writeValue v)

iProgramS :: Program -> [Integer] -> [Integer]
iProgramS prog ix = let (_,_,ox) = execState (iStmtS prog) (ix, [],[])  
                    in ox

--------------------- State applicative -----------------
eExprSa :: Expr -> State Work Integer
eExprSa (Var s)          = get >>= return . (getValue s) 
eExprSa (Const v)        = return v 
eExprSa (BinOp op e1 e2) = (applyBo op) <$> eExprSa e1 <*> eExprSa e2

iStmtSa :: Stmt -> State Work () 
iStmtSa (Assign var e)= eExprSa e >>= modify . (updValue var)
iStmtSa (If e s)      = eExprSa e >>= \v -> when (v>0) (iStmtSa s)
iStmtSa wh@(While e s)= eExprSa e >>= \v -> when (v>0) (iStmtSa s >> iStmtSa wh)
iStmtSa (Block vs sts)= modify (extMemory vs) >> 
                          mapM_ iStmtS sts >> modify (dropMemory (length vs))
iStmtSa (Read var)    = get >>= return . readInput >>= 
                          modify . (updValue var) >> modify dropInput  
iStmtSa (Write e)     = eExprS e >>=  modify . writeValue

iProgramSa :: Program -> [Integer] -> [Integer]
iProgramSa prog ix = let (_,_,ox) = execState (iStmtSa prog) (ix,[],[])  
                     in ox

iProgramMain :: SemanOpt -> Program -> [Integer] -> [Integer]  --   WorkO | StateO | ApplicO
iProgramMain r p il = case r of 
     ApplicO -> iProgramSa p il 
     StateO  -> iProgramS p il 
     WorkO   -> iProgram p il
