module PA1Helper(runProgram,Lexp(..),parseLEList) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Char

-- Haskell representation of lambda expression
-- In Lambda Lexp Lexp, the first Lexp should always be Atom String
data Lexp = Atom String | Lambda Lexp Lexp | Apply Lexp  Lexp 

-- Make it possible to determine if two lambda expressions are structurally equal
instance Eq Lexp  where
    (Atom v1) == (Atom v2) = v1 == v2
    (Lambda (Atom v1) exp1) == (Lambda (Atom v2) exp2) = v1 == v2 && exp1 == exp2
    (Apply exp1 exp2) == (Apply exp3 exp4) = exp1 == exp3 && exp2 == exp4  
    _ == _ = False

-- Allow for Lexp datatype to be printed like the Oz representation of a lambda expression
instance Show Lexp  where 
    show (Atom v) = v
    show (Lambda exp1 exp2) = "\\" ++ (show exp1) ++ "." ++ (show exp2) 
    show (Apply exp1 exp2) = "(" ++ (show exp1) ++ " " ++ (show exp2) ++ ")" 


-- Reserved keywords in Oz
-- P. 841 Table C.8, "Concepts, Techniques, and Models of Computer Programming", 
-- Van Roy, Haridi
ozKeywords = ["andthen","at","attr","break"
              ,"case","catch","choice","class"
              ,"collect","cond","continue"
              ,"declare","default","define"
              ,"dis","div","do","else"
              ,"elsecase","elseif","elseof"
              ,"end","export","fail","false"
              ,"feat","finally","for","from"
              ,"fun","functor","if","import"
              ,"in","lazy","local","lock"
              ,"meth","mod","not","of","or"
              ,"orelse","prepare","proc"
              ,"prop","raise","require"
              ,"return","self","skip","then"
              ,"thread","true","try","unit"
              ] 

-- Sparse language definition to define a proper Oz identifier
-- An atom is defined as follows:
-- 1. sequence of alphanumeric chars starting with a lowercase letter, 
--   excluding language keywords
-- 2. arbitrary printable chars enclosed in single quotes, 
--   excluding "'", "\", and "NUL"
-- lDef defines an atom as only 1.
-- P. 825,"Concepts, Techniques, and Models of Computer Programming", 
-- Van Roy, Haridi
lDef = emptyDef { identStart = lower
                , identLetter = alphaNum
                , reservedNames = ozKeywords
                } 
-- Obtains helper functions for parsing 
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reserved = m_reserved
           , brackets   = m_brackets
           } = makeTokenParser lDef

-- Below is code to parse Oz lambda expressions and represent them in Haskell
atom = do
  var <- m_identifier
  return (Atom var)

lambargs = do
  var1 <- atom
  char '.'
  var2 <- start
  return (Lambda var1 var2)

lamb = do
  char '\\'
  p <- lambargs
  return p

appargs = do
    var1 <- start
    spaces
    var2 <- start
    return (Apply var1 var2)

app = do
  p <- m_parens appargs 
  return p

start = atom <|> lamb <|> app

-- Use previously defined parser to parse a given String
parseLExpr :: String -> Either ParseError Lexp 
parseLExpr input = parse start "" input

-- Given a list of Strings, return a list containing only Lexp objects
-- for strings that are valid lambda expressions
parseLEList :: [String]->[Lexp]
parseLEList [] = []
parseLEList (x:xs) = let xs' = parseLEList xs 
                              in case (parseLExpr x) of
                                   Left err -> xs' 
                                   Right x' -> (x':xs') 

-- Pretty printer for outputting inputted lambda expressions along with
-- their reduced expressions. Integer used to distiguish between test cases.
outputPrinter :: Int -> [(Lexp,Lexp)] -> IO()
outputPrinter _ [] = return ()
outputPrinter n ((lexp,lexp'):lexps) = do
    putStrLn ("Input " ++ (show n) ++ ": " ++ (show lexp))
    putStrLn ("Result " ++ (show n) ++ ": " ++ (show lexp'))
    outputPrinter (n+1) lexps

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
runProgram :: String -> (Lexp -> Lexp) -> IO()
runProgram fileName reducer = do
    fcontents <- readFile fileName
    let inList = lines fcontents 
    let parsedList = parseLEList inList
    let reducedList = map reducer parsedList
    outputPrinter 1 (zip parsedList reducedList)   


