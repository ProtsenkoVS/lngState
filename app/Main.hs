module Main where

import Syntax 
import Semantics 
import Parse
---------------------
import Text.ParserCombinators.Parsec()
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as CE
---------------------
import Options.Applicative
import Data.Semigroup ((<>))

main :: IO()
main = runInputT defaultSettings loop 
   where  loop :: InputT IO()
          loop = do input <- getInputLine "lngState>" 
                    case input of 
                      Nothing -> return ()
                      Just "" -> return ()
                      Just st -> do liftIO $ either putStrLn readInterpret (findParameters1 st) 
                                    loop 

readInterpret :: (ParseOpt,SemanOpt,String,[Integer]) -> IO()
readInterpret (p,s,file,ix) = do 
  fl <- readFile file
  CE.catch (print (interpretFull p s fl ix)) hError

hError :: CE.ErrorCall -> IO()
hError (CE.ErrorCallWithLocation se _)  = putStrLn se

interpretFull :: ParseOpt -> SemanOpt -> String -> [Integer] -> [Integer]
interpretFull p s st ix = let pr = parseLS p st 
                          in  iProgramMain s pr ix

-- only in ghci :for testing 
interpret :: String -> [Integer] -> [Integer]
interpret st ix = let pr = parseLSL st 
                  in  iProgramS pr ix   

------ Робота з параметрами : ВВедення програми + даних --------------------
workParameters :: String -> ParserResult (ParseOpt,SemanOpt,String,[String])  
workParameters  str = execParserPure Options.Applicative.defaultPrefs opts (words str) 
    where opts = info (parameters <**> helper)
                 (  fullDesc 
                 <> progDesc "Interpret LS"
                 <> header "lngState-exe - program-interpreter fo LS")

findParameters1 :: String -> Either String (ParseOpt,SemanOpt,String,[Integer])
findParameters1 str = case workParameters str of 
                        Success (po,so,pr,dx) -> Right (po, so, pr, map read dx)
                        Failure ps            -> Left (fst (renderFailure ps ""))      
                        CompletionInvoked _   -> Left "CompletionInvoked"

parameters :: Parser (ParseOpt,SemanOpt,String,[String]) 
parameters = (,,,)                                       
   <$> (parserOption <|> libraryOption)               
   <*> (workOption <|> stateOption <|> applicOption)  
   <*> argument str (metavar "FileProgram")
   <*> some (argument str (metavar "DataForProgram..."))   

parserOption :: Parser ParseOpt 
parserOption = flag ParserO ParserO 
    ( short 'P'
   <> help "parse: Parser + Do notation")

libraryOption :: Parser ParseOpt 
libraryOption = flag' LibraryO 
    ( short 'L'
   <> help "parse: Library + Applicative") 

workOption :: Parser SemanOpt 
workOption = flag WorkO WorkO
    (short 'W' 
   <> help "semantics: Work is parameter") 

stateOption :: Parser SemanOpt 
stateOption = flag' StateO 
    ( short 'S'
   <> help "semantics: State with Work") 

applicOption :: Parser SemanOpt 
applicOption = flag' ApplicO 
    ( short 'A'
   <> help "semantics: State + Applicative") 

{-  S P file i1 ... ik
         P = -P - parser + do notation
		     -L - library + aplicative
         S = -W - Work - parameter
             -S - State 
             -A - State + aplicative 
         file - file with program 
         i1 ... ik - input data		 
-}