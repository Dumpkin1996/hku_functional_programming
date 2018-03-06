import Data.Char
import Data.List
import Parsing hiding (ident, identifier)
import System.IO

ident :: Parser String
ident = do
    x  <- (lower +++ upper)
    xs <- many alphanum
    return (x:xs)

identifier  :: Parser String
identifier  =  token ident

-- Problem 1 
lift :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift f Nothing _ = Nothing
lift f _ Nothing = Nothing
lift f (Just a) (Just b) = Just (f a b)
-- lift f x y = f <$> x <*> y

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr| Div Expr Expr | Mod Expr Expr | Val Int | Var String deriving (Eq, Show)

type Env = [(String, Int)]

-- Problem 2
eval :: Env -> Expr -> Maybe Int
eval env (Add e1 e2) = lift (+) (eval env e1) (eval env e2)
eval env (Sub e1 e2) = lift (-) (eval env e1) (eval env e2)
eval env (Mul e1 e2) = lift (*) (eval env e1) (eval env e2)
eval env (Div e1 e2) = if eval env e2 == Just 0 then Nothing else lift (div) (eval env e1) (eval env e2)
eval env (Mod e1 e2) = if eval env e2 == Just 0 then Nothing else lift (mod) (eval env e1) (eval env e2)
eval env (Val a) = Just a
eval env (Var s) = lookup s env

-- Problem 3
pExpr :: Parser Expr
pExpr = do
    t <- pTerm
    pOp_term t

pOp_term :: Expr -> Parser Expr
pOp_term t1 = pAdd t1 +++ pSub t1 +++ (return t1)
    where
        pAdd t1 = do 
            token $ char '+'
            t2 <- pTerm
            pOp_term (Add t1 t2)
        pSub t1 = do
            token $ char '-'
            t2 <- pTerm
            pOp_term (Sub t1 t2)

pTerm :: Parser Expr
pTerm = do
    f <- pFactor
    pOp_factor f

pOp_factor :: Expr -> Parser Expr
pOp_factor f1 = pMul f1 +++ pDiv f1 +++ pMod f1 +++ (return f1)
    where
        pMul f1 = do
            token $ char '*'
            f2 <- pFactor
            pOp_factor (Mul f1 f2)
        pDiv f1 = do
            token $ char '/'
            f2 <- pFactor
            pOp_factor (Div f1 f2)
        pMod f1 = do
            token $ char '%'
            f2 <- pFactor
            pOp_factor (Mod f1 f2)                     

pFactor :: Parser Expr
pFactor = pPara +++ pInt +++ pVar
    where
        pPara = do
            _ <- token $ char '('
            e <- pExpr
            _ <- token $ char ')' 
            return $ e
        pInt = do x <- integer
                  return $ Val x
        pVar = do x <- identifier
                  return $ Var x

-- Problem 4
runParser :: Parser a -> String -> Maybe a
runParser p inp = 
    case (parse p inp) of 
        [(n,[])]   -> Just n
        [(_,rest)] -> Nothing
        []         -> Nothing

type Table = [[Maybe Int]]

position :: Eq a => a -> [a] -> Int
position = go 0
    where
        go :: Eq a => Int -> a -> [a] -> Int
        go _ _ [] = -1
        go i v (x:xs) = if v == x then i else go (i + 1) v xs

intToColumn :: Int -> String
intToColumn 0 = []
intToColumn n = intToColumn (m `div` 26) ++ [alphabet !! (m `mod` 26)]
    where
        m = n - 1
        alphabet = ['A'..'Z']

columnToInt :: String -> Int
columnToInt [] = 0
columnToInt (x:xs) = first * (26 ^ length xs) + columnToInt xs
    where
        alphabet = ['A'..'Z']
        first = 1 + position x alphabet

-- Problem 5
width :: Maybe Int -> Int
width Nothing = 0
width (Just x) = length (show x)

setWidth :: Table -> [Int]
setWidth t = [width (Just m)] ++ [ maximum ( [width (xs!!(i-1)) | xs <- t] ++ [length (intToColumn i)] ) | i <- [1..n] ]
    where
        m = length t
        n = length (head t) 

withWidth :: String -> Int -> String
withWidth s w = (replicate (w - (length s)) ' ') ++ s

printhead :: Int -> [Int] -> IO ()
printhead _ [] = do putStrLn ""
printhead j widths = do
    putStr ( withWidth (intToColumn j) (head widths) )
    putChar '|'
    printhead (j+1) (tail widths)

printline :: [Int] -> [Maybe Int] -> IO ()
printline _ [] = putStrLn ""
printline [] _ = putStrLn ""
printline widths l = do
    putStr (withWidth (f (head l)) (head widths))
    putChar '|'
    printline (tail widths) (tail l)
    where
        f Nothing = ""
        f (Just x) = show x

printcontent :: Int -> [Int] -> Table -> IO()
printcontent _ _ [] = return ()
printcontent i w t = do
    putStr (withWidth (show i) (head w))
    putChar '|'
    printline (tail w) (head t)
    printcontent (i+1) w (tail t)

printTable :: Table -> IO ()
printTable t = do
    printhead 0 widths
    printcontent 1 widths t
    where
        widths = setWidth t

-- Problem 6,7,8
getrownum :: IO Int
getrownum = do
    putStr "Number of rows: "
    s <- getLine
    case parse integer s of
        [(x,"")] -> return x
        _ -> getrownum

getcolnum :: IO Int
getcolnum = do
    putStr "Number of columns: "
    s <- getLine
    case parse integer s of
        [(x,"")] -> return x
        _ -> getcolnum

data Command = Table | Quit | Vars | Assign String Expr | Del String

pCommand :: Parser Command
pCommand = pTable +++ pQuit +++ pVars +++ pAssign +++ pDel
  where
    pTable = do
        string "table"
        return Table
    pQuit = do
        string "quit"
        return Quit
    pVars = do -- Problem 8
        string "vars"
        return Vars
    pAssign = do
        s <- identifier
        token $ char '='
        e <- pExpr
        return $ Assign s e
    pDel = do -- Problem 7
        token $ string "del"
        s <- identifier
        return $ Del s

locator :: Parser (Int, Int)
locator = do
    s <- many upper
    x <- integer
    return $ (x, (columnToInt s))

updateEnv :: Env -> String -> Int -> Env
updateEnv env p x = 
    case lookup p env of
        Nothing -> env ++ [(p, x)]
        _ -> [ if s == p then (s,x) else (s,i) | (s,i) <- env]

updateTable :: Table -> String -> Maybe Int -> Table
updateTable t p a = [ if i == px then (updateLine (t!!(i-1)) py a) else t!!(i-1) | i <- [1..m] ] 
    where
        m = length t 
        px = (fst (fst (head (parse locator p))))
        py = (snd (fst (head (parse locator p)))) 

updateLine :: [Maybe Int] -> Int -> Maybe Int -> [Maybe Int]
updateLine line py a = [ if j == py then a else line!!(j-1) | j <- [1..n]]
    where
        n = length line

printVars :: [(String, Int)] -> IO () -- Problem 8
printVars [] = return ()
printVars ((s,i) : xs) = do
    putStrLn (s ++ " = " ++ (show i))
    printVars xs

updateVars :: [(String, Int)] -> String -> Maybe Int -> [(String, Int)] -- Problem 8
updateVars v n a = 
    case a of
        Nothing -> delete (n,x) v
            where Just x = lookup n v
        Just x ->
            case lookup n v of
                Nothing -> v ++ [(n,x)]
                _ -> [ if s == n then (s,x) else (s,i) | (s,i) <- v]

repl :: Env -> Table -> [(String, Int)] -> IO ()
repl env t v = do
    putStr "\n>"
    s <- getLine
    case parse pCommand s of
        [(Quit, "")] -> return ()
        [(Table, "")] -> do
            printTable t
            repl env t v
        [(Vars, "")] -> do -- Problem 8
            printVars $ sort v
            repl env t v
        [(Assign p e, "")] ->
                case eval env e of
                    Nothing -> do
                        -- putStrLn "Error, Invalid expression!"
                        putStrLn "Error"
                        repl env t v
                    Just x ->
                        if (isUpper (head p)) then
                            if ( (fst (fst (head (parse locator p)))) > m || (snd (fst (head (parse locator p)))) > n )
                                then do
                                    -- putStrLn "Error, Index out of range!"
                                    putStrLn "Error"
                                    repl env t v
                                else do
                                    putStrLn (p ++ " = " ++ (show x))
                                    repl (updateEnv env p x) (updateTable t p (Just x)) v 
                        else do -- Peoblem 8
                            putStrLn (p ++ " = " ++ (show x))
                            repl (updateEnv env p x) t (updateVars v p (Just x))              
        [(Del p, "")] -> -- Problem 7
            case lookup p env of
                Nothing -> do
                    -- putStrLn "Error, Not found!"
                    putStrLn "Error"
                    repl env t v
                Just x -> do
                    putStrLn ("Deleted " ++ p)
                    if (isUpper (head p)) then 
                        do repl (delete (p,x) env) (updateTable t p Nothing) v
                    else
                        do repl (delete (p,x) env) t (updateVars v p Nothing)

        _ -> do
            -- putStrLn "Error, Illegal command!"
            putStrLn "Error"
            repl env t v
    where
        m = length t
        n = length (head t)
               
main :: IO()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    m <- getrownum
    n <- getcolnum
    repl [] (replicate m (replicate n Nothing)) []
