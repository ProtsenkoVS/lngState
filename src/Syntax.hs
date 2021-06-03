{-# OPTIONS_GHC -Wall #-}
module Syntax where 

data Expr = Var String          -- Змінна
          | Const Integer       -- константа
          | BinOp Op Expr Expr  -- Операція
             deriving (Show, Eq)
-- Бінарні (2-аргумента) оператори
data Op =  Plus | Minus | Times | Div | Mod  
             deriving (Show, Eq)
data Stmt = Assign String Expr
          | Read String 
          | Write Expr
          | If Expr Stmt 
          | While Expr Stmt       
          | Block [String] [Stmt]        
             deriving (Show, Eq)
--  В операторах if (iv) s і while (iv) s значення v>0 цілого виразу iv 
--                еквівалентно логічному значенню True 			 
type Program = Stmt

type Work = ([Integer], [(String,Integer)], [Integer])

data ParseOpt = ParserO | LibraryO deriving (Eq, Show) 
data SemanOpt = WorkO | StateO | ApplicO deriving (Eq, Show)

-- Програми -------------------------------------------
{- Вводить два цілі значення b і e, 
   якщо вони додатні, то в змінній out
   обчислення значення b в степені e, 
   в інших випадках значення змінної out=0. 
   Значення out виводиться

   { int b, e, out;  
     read b; read e; out:= 0;
	 if (b)
       if (e)
	     {int i; i:=0; out := 1; 
		  while (e-i) {out := out*b; i := i+1}; }; 
     write out  
   }
-}
power :: Program
power = Block ["b","e","out"]
              [ Read "b", Read "e", Assign "out" (Const 0)
              , If (Var "b") 
                  (If (Var "e")
                    (Block ["i"]
                        [ Assign "i" (Const 0), Assign "out" (Const 1)
                        , While (BinOp Minus (Var "e") (Var "i"))
                           (Block [] [ Assign "out" (BinOp Times (Var "out") (Var "b"))
                                    , Assign "i" (BinOp Plus (Var "i") (Const 1))])
                        ]
                     )
                  )
              , Write (Var "out")
              ]

{- Вводить ціле значення in,
   якщо воно невід"ємне, то обчислює відповідне число 
   Фібонначі в змінній out і виводить його.
   При від"ємному in виводиться 0.
   
   {int in, out; 
    read in; out := 0; 
	if (in+1)                            -- in>=0 
      {int f0, f1, c; 
	   f0 := 1; f1 := 1; out := 1;
       if (in-1)                       -- in>1
         { c := 1; while (in-c) 
		   {out := f0 + f1; f0 := f1; f1 := out; c := c+1}
		 } 
	 };
    write out	 
  }
-}
fibonacci :: Program
fibonacci = 
    Block ["in", "out"]
          [ Read "in",  Assign "out" (Const 0)
          , If (BinOp Plus (Var "in") (Const 1)) 
               (Block ["f0", "f1", "c"]
                      [Assign "f0" (Const  1), Assign "f1" (Const 1), Assign "out" (Const 1),
                       If (BinOp Minus (Var "in") (Const 1))
                         (Block [] [ Assign "c" (Const 1)
                                   , While (BinOp Minus (Var "in") (Var "c")) 
                                       (Block []
                                              [ Assign "out" (BinOp Plus (Var "f0") (Var "f1"))
                                              , Assign "f0" (Var "f1")
                                              , Assign "f1" (Var "out")
                                              , Assign "c" (BinOp Plus (Var "c") (Const 1))
                                              ]
                                       )
                                   ]
                         )
                     ]
               )
          , Write (Var "out")
          ]

powerTest :: String
powerTest =
   "{ int b, e, out; \n\
   \  read b; read e; out:= 0;\n\
   \  if (b) \n \
   \    if (e)\n \
   \     {int i; i:=0; out := 1;\n\
   \      while (e-i) {out := out*b; i := i+1} }; \n\
   \  write out \n\
   \}"

fibonacciTest :: String
fibonacciTest = 
 " {int in, out; read in; out := 0; \n\
   \if (in+1){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in-1) \n \
   \              { c := 1; while (in-c)  \n\
   \                  {out := f0 + f1; f0 := f1; f1 := out; c := c+1}\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}" 

{-
program=  stmt ;
stmt   = 'while' , '(' , expr , ')' , stmt ; 
       | 'if' , '(' , expr , ')' , stmt ;
       | 'read' , identifier ; 
       | 'write' , expr ; 
       | identifier , ':=' , expr ;
       | '{' , [defin] ,  stmt , {';' , stmt} , '}' ;
defin  = 'int' , identifier , {',' , identifier} , ';' ;
expr   = term , {addOp , term} ;
term   = factor , {mulOp , factor} ;
factor = decimal | '(' , expr , ')' | identifier ;
addOp  = '+' | '_' ;
mulOp  = '*' | '/' | '%' ;
identifier = letter , {(digit | letter)} ;  
             крім 'int' 'if' 'while' 'read' 'write' 
decimal = digit , {digit} ;
letter  = 'A' | ... | 'Z' | 'a' | ... | 'z' ;
digit   = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ; 
-}
{-
data WorkSp = WorkSp { input :: [Integer]
                     , memory :: [(String,Integer)]
                     , output :: [Integer]}
-}