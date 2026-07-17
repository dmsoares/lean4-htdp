namespace GenerativeRecursion.TraversingGraphs

def Node := String
  deriving BEq

def Neighbours := List Node
  deriving BEq

def Entry := Node × Neighbours
  deriving BEq

def Graph := List Entry
  deriving BEq

def Path := List Node
  deriving BEq

def g : Graph := [
  ("A", ["B", "E"]),
  ("B", ["E", "F"]),
  ("C", ["D"]),
  ("D", []),
  ("E", ["C", "F"]),
  ("F", ["D", "G"]),
  ("G", [])
]

def entry : Entry := ("F", ["D", "G"])
def target : List Node := entry.snd

def neighbours (n : Node) (g : Graph) : Neighbours :=
  Option.getD
    (Prod.snd <$> List.find? pred g)
    ([] : List Node)
  where
    pred := fun ((s, _) : Entry) => s == n

#guard neighbours "B" [] == []
#guard neighbours "B" g == ["E", "F"]

def optionMap {a} {b} (f : a -> Option b) (list : List a) : Option b :=
  match list with
    | [] => none
    | (a :: as) =>
      let res := f a
      match res with
        | none => optionMap f as
        | _ => res

partial def findPath
  (origin : Node)
  (dest : Node)
  (neighbours : Node -> Graph -> Neighbours)
  (g : Graph) : Option Path :=
    if origin == dest
    then some [origin]
    else do
      let next := neighbours origin g
      let candidate <- findPathOnList next dest g
      pure $ List.append [origin] candidate
    where
      findPathOnList (ns : Neighbours) (dest : Node) (g : Graph) : Option Path :=
        flip optionMap ns $ fun n => findPath n dest neighbours g

#guard findPath "A" "B" neighbours [] == none
#guard findPath "C" "G" neighbours g ==  none
#guard findPath "C" "D" neighbours g == some ["C", "D"]
#guard (findPath "E" "D" neighbours g) == some ["E", "C", "D"]
#guard (findPath "A" "G" neighbours g) == some ["A", "B", "E", "F", "G"]

end GenerativeRecursion.TraversingGraphs
