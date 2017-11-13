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
