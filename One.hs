data Expression = Var String
  | Const Int
  | Add Expression Expression
  | Mul Expression Expression deriving(Show)

ex1 = Add (Mul (Const 2) (Var "x")) (Var "y")

simplify :: Expression -> Expression
simplify (Add (Const a) (Const b)) = Const (a + b)
simplify (Mul (Const x) (Const y)) = Const(x * y)
simplify (Add (Const 0) x) = x
simplify (Add x (Const 0)) = x
simplify (Mul (Const 0) x) = (Const 0)
simplify (Mul x (Const 0)) = Const(0)
simplify (Mul (Const 1) x) = x
simplify (Mul x (Const 1)) = x
simplify x = x

recSimplify :: Expression -> Expression
recSimplify (Add x y) = simplify((Add (recSimplify x) (recSimplify y)))
recSimplify (Mul x y) = simplify((Mul (recSimplify x) (recSimplify y)))
recSimplify x = x

ex2 = (Add (Mul (Add (Mul (Const 0) (Var "x")) (Const 1)) (Const 3)) (Const 12))

space x = (elem x " \t\n\r")
punctuation x  = elem x "()[]{},"
symbolic x = elem x "~`!@#$%^&*-+=|\\:;<>.?/"
numeric x = elem x "0123456789"
alphanumeric x = elem x "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

lexwhile :: (Char -> Bool) -> String -> (String, String)
lexwhile property input
  | length input == 0     = ("", "")
  | property (head input) = ((head input):x, y)
  | otherwise             = ("", input)
  where (x,y) = lexwhile property (tail input)


lexit :: String -> [String]
lexit "" = []
lexit input =
  let c:cs = snd (lexwhile space input)
      prop = if (alphanumeric c) then alphanumeric
             else if (symbolic c) then symbolic
             else \x -> False
      (tok1, rest) = lexwhile prop cs 
  in (c:tok1) : (lexit rest)

ex3 = "2*((var_1 + x') + 11)"
ex4 = "if (*p1-- == *p2++) then f() else g()"


parseExpression :: [String] -> (Expression, [String])
parseExpression input =
  let (exp1, tok1) = parseProduct input
  in case tok1 of "+":xs -> let (exp2, tok2) = parseExpression xs in ((Add exp1 exp2), tok2)
                  _ -> (exp1, tok1)


parseProduct input =
  let (exp1, tok1) = parseAtom input
  in case tok1 of "*":xs -> let (exp2, tok2) = parseProduct xs in ((Mul exp1 exp2), tok2)
                  _ -> (exp1, tok1)


parseAtom [] = error "Expected an expression at end of input"
parseAtom ("(":xs) = let out = parseExpression xs
                   in case out of (x, ")":tok) -> (x, tok)
                                  x -> error "Expected closing bracket"
parseAtom (x:xs)
  | numeric (head x) = (Const(read x :: Int), xs)
  | otherwise = (Var(x),xs)

