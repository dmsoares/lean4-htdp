namespace Accumulators.Path

def Node := String
  deriving BEq, Inhabited
def Connection := Node × Node
  deriving BEq

def SimpleGraph := List Connection
  deriving BEq

def getNeighbour (n : Node) (g : SimpleGraph) : Node :=
  match g.find? (λ (s, _) => s == n) with
    | none => panic! "no_neighbour"
    | some (_, t) => t

def Visited := List Node
def visited? (v : Visited) := v.elem

partial def pathExists (source : Node) (target : Node) (graph : SimpleGraph) : Bool :=
  checkPath [source] source target graph
  where
    checkPath visited source target graph :=
      source == target ||
        let neighbour := getNeighbour source graph
        not (visited? visited neighbour) && checkPath (neighbour :: visited) neighbour target graph

def graph :=
  [("A", "B"),
   ("B", "C"),
   ("C", "E"),
   ("D", "E"),
   ("E", "B"),
   ("F", "F")]

#guard pathExists "A" "E" graph == true
#guard pathExists "A" "F" graph == false

def fl (f : a -> b -> b) (i : b) (l : List a) : b :=
  match l with
    | [] => i
    | (x :: xs) => fl f (f x i) xs

def fr (f : a -> b -> b) (i : b) (l : List a) : b :=
  match l with
    | [] => i
    | (x :: xs) => f x $ fr f i xs


#eval fl (λ a b => a :: b) [] [1, 2, 3]

end Accumulators.Path
