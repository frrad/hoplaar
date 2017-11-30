data Formula a
  = True
  | False
  | Atom a
  | Not (Formula a)
  | And (Formula a)
        (Formula a)
  | Or (Formula a)
       (Formula a)
  | Imp (Formula a)
        (Formula a)
  | Iff (Formula a)
        (Formula a)
  | Forall String
           (Formula a)
  | Exists String
           (Formula a)
