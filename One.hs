data Expression
  = Var String
  | Const Int
  | Add Expression
        Expression
  | Mul Expression
        Expression

ex1 :: Expression
ex1 = Add (Mul (Const 2) (Var "x")) (Var "y")

simplify :: Expression -> Expression
simplify (Add (Const a) (Const b)) = Const (a + b)
simplify (Mul (Const x) (Const y)) = Const (x * y)
simplify (Add (Const 0) x) = x
simplify (Add x (Const 0)) = x
simplify (Mul (Const 0) _) = (Const 0)
simplify (Mul _ (Const 0)) = Const (0)
simplify (Mul (Const 1) x) = x
simplify (Mul x (Const 1)) = x
simplify x = x

recSimplify :: Expression -> Expression
recSimplify (Add x y) = simplify ((Add (recSimplify x) (recSimplify y)))
recSimplify (Mul x y) = simplify ((Mul (recSimplify x) (recSimplify y)))
recSimplify x = x

ex2 :: Expression
ex2 = (Add (Mul (Add (Mul (Const 0) (Var "x")) (Const 1)) (Const 3)) (Const 12))

space :: Char -> Bool
space x = (elem x " \t\n\r")

punctuation :: Char -> Bool
punctuation x = elem x "()[]{},"

symbolic :: Char -> Bool
symbolic x = elem x "~`!@#$%^&*-+=|\\:;<>.?/"

numeric :: Char -> Bool
numeric x = elem x "0123456789"

alphanumeric :: Char -> Bool
alphanumeric x =
  elem x "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

lexwhile :: (Char -> Bool) -> String -> (String, String)
lexwhile property input
  | length input == 0 = ("", "")
  | property (head input) = ((head input) : x, y)
  | otherwise = ("", input)
  where
    (x, y) = lexwhile property (tail input)

lexit :: String -> [String]
lexit "" = []
lexit input =
  let c:cs = snd (lexwhile space input)
      prop =
        if (alphanumeric c)
          then alphanumeric
          else if (symbolic c)
                 then symbolic
                 else \_ -> False
      (tok1, rest) = lexwhile prop cs
  in (c : tok1) : (lexit rest)

ex3 :: String
ex3 = "2*((var_1 + x') + 11)"

ex4 :: String
ex4 = "if (*p1-- == *p2++) then f() else g()"

parseExpression :: [String] -> (Expression, [String])
parseExpression input =
  let (exp1, tok1) = parseProduct input
  in case tok1 of
       "+":xs ->
         let (exp2, tok2) = parseExpression xs
         in ((Add exp1 exp2), tok2)
       _ -> (exp1, tok1)

parseProduct :: [String] -> (Expression, [String])
parseProduct input =
  let (exp1, tok1) = parseAtom input
  in case tok1 of
       "*":xs ->
         let (exp2, tok2) = parseProduct xs
         in ((Mul exp1 exp2), tok2)
       _ -> (exp1, tok1)

parseAtom :: [String] -> (Expression, [String])
parseAtom [] = error "Expected an expression at end of input"
parseAtom ("(":xs) =
  let out = parseExpression xs
  in case out of
       (x, ")":tok) -> (x, tok)
       _ -> error "Expected closing bracket"
parseAtom (x:xs)
  | numeric (head x) = (Const (read x :: Int), xs)
  | otherwise = (Var (x), xs)

makeParser :: ([String] -> (Expression, [String])) -> String -> Expression
makeParser parseFn s =
  let (expr, rest) = (parseFn (lexit s))
  in if rest /= []
       then error "Unparsed input"
       else expr

defaultParser :: String -> Expression
defaultParser = makeParser parseExpression

ex5 :: String
ex5 = "x + 1"

ex6 :: String
ex6 = "(x1 + x2 + x3) * (1+2+3*x+ y)"

stringOfExp :: Expression -> String
stringOfExp (Var x) = x
stringOfExp (Const x) = show x
stringOfExp (Add x y) = "(" ++ (stringOfExp x) ++ "+" ++ (stringOfExp y) ++ ")"
stringOfExp (Mul x y) = "(" ++ (stringOfExp x) ++ "*" ++ (stringOfExp y) ++ ")"

ex7 :: Expression
ex7 = defaultParser "x + 3 * y"

stringOfExpPr :: Int -> Expression -> String
stringOfExpPr _ (Var x) = x
stringOfExpPr _ (Const x) = show x
stringOfExpPr priority (Add x y) =
  let s = (stringOfExpPr 3 x) ++ " + " ++ (stringOfExpPr 2 y)
  in if 2 < priority
       then "(" ++ s ++ ")"
       else s
stringOfExpPr priority (Mul x y) =
  let s = (stringOfExpPr 5 x) ++ " * " ++ (stringOfExpPr 4 y)
  in if 4 < priority
       then "(" ++ s ++ ")"
       else s

instance Show Expression where
  show x = "<<" ++ (stringOfExpPr 0 x) ++ ">>"
