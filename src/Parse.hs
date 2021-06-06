{-# OPTIONS_GHC -Wall #-}
module Parse where

import Text.ParserCombinators.Parsec
import Syntax
----------------------
import qualified Text.ParserCombinators.Parsec.Expr as E
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

{- лексика  
  symbol = ';' | '{' | '}' | '(' | ')' | ','
  addOp  = '+' | '_' ;
  mulOp  = '*' | '/' | '%' ;
  identifier = letter , {(digit | letter)} ;  
             крім 'int' 'if' 'while' 'read' 'write' 
  decimal = digit , {digit} ;
  letter  = 'A' | ... | 'Z' | 'a' | ... | 'z' ;
  digit   = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ; 
  identif =  char {digit | char}
  reserved= 'int' | 'read' | 'write' | 'if' | 'while' 
-}

identif :: Parser String
identif = do c  <- letter 
             cs <- many alphaNum  
             return (c:cs)

lexem :: Parser a -> Parser a
lexem p = do a <- p
             spaces
             return a      

identifier :: Parser String
identifier = try (do nm <- lexem identif
                     if (any(nm==) ["int","read","write","if","while"])
                         then unexpected ("reserved word " ++ show nm)
                         else return nm 
                   ) 

reserved :: String -> Parser ()
reserved st = try( lexem (do _ <- string st
                             notFollowedBy alphaNum)) 

symbol :: String ->  Parser ()
symbol st = lexem $ do _ <- string st
                       return ()

{- вирази 
  expr   = term , {addOp , term} ;
  term   = factor , {mulOp , factor} ;
  factor = decimal | '(' , expr , ')' | identifier ;
-}

oper  :: String -> Op -> Parser (Expr -> Expr -> Expr)
oper str bop = do symbol str
                  return $ BinOp bop 

mulOp, addOp :: Parser (Expr -> Expr -> Expr)   
mulOp = (oper "*" Times) <|> (oper "/" Div) <|> (oper "%" Mod)
addOp = (oper "+" Plus) <|> (oper "-" Minus)

parens :: Parser a -> Parser a
parens p = do symbol "("
              e <- p
              symbol ")"
              return e 

decimal :: Parser Integer
decimal = do ds  <- many1 digit
             return (read ds) 

factor, term, expr :: Parser Expr
factor  = parens expr 
       <|> do v <- lexem decimal
              return (Const v) 
       <|> do v <- identifier
              return (Var v)
term = chainl1 factor mulOp   
expr = chainl1 term addOp 

{- оператори
  program=  stmt ;
  stmt   = 'while' , '(' , expr , ')' , stmt ; 
         | 'if' , '(' , expr , ')' , stmt ;
         | 'read' , identifier ; 
         | 'write' , expr ; 
         | identifier , ':=' , expr ;
         | '{' , [defin] ,  stmt , {';' , stmt} , '}' ;
  defin  = 'int' , identifier , {',' , identifier} , ';' ;
-}   

braces :: Parser a -> Parser a
braces p = do {_ <- symbol "{"; e <- p; _ <- symbol "}"; return e} 

semiSep1, commaSep1 :: Parser a -> Parser [a] 
semiSep1 p = sepBy p (symbol ";")
commaSep1 p = sepBy p (symbol ",")

stmt :: Parser Stmt 
stmt = do reserved "while"
          e <- parens expr
          s <- stmt
          return (While e s)
   <|> do reserved "if"
          e <- parens expr
          s <- stmt
          return (If e s)
   <|> do reserved "read"
          v <- identifier    
          return (Read v)
   <|> do reserved "write"
          e <- expr
          return (Write e) 
   <|> do var <- identifier
          symbol ":="
          e <- expr
          return (Assign var e)
   <|> braces (do dll <- option [] defin
                  sl <- semiSep1 stmt
                  return (Block dll sl))
   
defin :: Parser [String] 
defin = do reserved "int"
           il <- commaSep1 identifier
           symbol ";"
           return il
   
program :: Parser Stmt 
program = do spaces
             r <- stmt
             eof
             return r

--------------------------------------------------
lexDef :: P.LanguageDef()
lexDef = emptyDef {identStart = letter
                  ,identLetter = alphaNum 
                  ,reservedNames = ["int","read","write","if","while"]
                  ,reservedOpNames = ["*","/","%","+","-"]}

lexer :: P.TokenParser ()
lexer =  P.makeTokenParser lexDef 

factorL :: Parser Expr
factorL  =  P.parens lexer expr 
        <|> (P.decimal lexer >>= return . Const)
        <|> (P.identifier lexer >>= return . Var) 

table :: E.OperatorTable Char () Expr
table  = [[op "*" Times E.AssocLeft, op "/" Div E.AssocLeft, op "%" Mod E.AssocLeft]
         ,[op "+" Plus E.AssocLeft, op "-" Minus E.AssocLeft]
         ]
         where
            op s f assoc = E.Infix (binOp s f) assoc  
            binOp s f = P.reservedOp lexer s >> return (Syntax.BinOp f) 

exprL :: Parser Expr
exprL  = E.buildExpressionParser table factorL  

stmtL :: Parser Stmt 
stmtL = (P.reserved lexer "while" >> While <$> P.parens lexer exprL <*> stmtL)
    <|> (P.reserved lexer "if" >> If <$> P.parens lexer exprL <*> stmtL)
    <|> (P.reserved lexer "read" >> Read <$> P.identifier lexer)
    <|> (P.reserved lexer "write">> Write <$> exprL)
    <|> (P.identifier lexer >>= (\var -> P.symbol lexer ":=" >> Assign var <$> exprL))
    <|> (P.braces lexer (Block <$> (option [] definL) <*> P.semiSep1 lexer stmtL))  --blockSt

definL :: Parser [String] 
definL = P.reserved lexer "int" *> P.commaSep1 lexer (P.identifier lexer) <* P.symbol lexer ";"

programL :: Parser Program 
programL = P.whiteSpace lexer *> stmtL <* eof -- {P.whiteSpace lexer; r <- stmt; eof; return r}

parseLS :: ParseOpt -> String -> Program  -- ParserO | LibraryO
parseLS p s = let r = if p==ParserO then parse program "" s
                                    else parse programL "" s  
              in case r of
                 Left _   -> error "Syntax" 
                 Right pr -> pr

parseLSL :: String -> Program
parseLSL s = case parse programL "" s of
               Left _  -> error "Syntax"
               Right e -> e
