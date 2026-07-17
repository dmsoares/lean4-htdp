namespace BST

inductive Tree where
  | None : Tree
  | Node (v : Int) (l : Tree) (r : Tree)

def prettyTree (t : Tree) : String :=
  match t with
  | .None => "None"
  | .Node v l r =>
      "(Node " ++ toString v ++ " " ++ prettyTree l ++ " " ++ prettyTree r ++ ")"

def size (t : Tree) : Nat :=
  match t with
  | .None => 0
  | .Node _ l r => 1 + size l + size r

def createBst (n: Nat) (t: Tree) : Tree :=
  match t with
  | .None => .Node n .None .None
  | .Node v l r =>
    if n < v
      then .Node v (createBst n l) r
      else .Node v l (createBst n r)

def createBstFromList (ns : List Nat) : Tree :=
  match ns with
  | [] => .None
  | (n :: ns) =>
      createBst n (createBstFromList ns)

def t : Tree := .Node 0 .None .None
def t' : Tree := .Node 2 .None .None
def t'' : Tree := .Node 1 t t'

def run : IO Unit := do
  IO.println (prettyTree t'')
  -- IO.println (prettyTree (createBst 3 t''))
  IO.println (prettyTree (createBstFromList [0, 1, 2]))
  IO.println (prettyTree (createBstFromList [1, 2, 0]))

end BST
