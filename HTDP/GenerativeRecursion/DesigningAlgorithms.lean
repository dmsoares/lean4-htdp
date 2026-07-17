namespace GenerativeRecursion.DesigningAlgorithms

def insertionSort [LE a] [DecidableRel (α := a) (· ≤ ·)] (xs : List a) : List a :=
  match xs with
    | [] => []
    | (x :: rest) => insert x (insertionSort rest)
  where
    insert [LE a] [DecidableRel (α := a) (· ≤ ·)] (x : a) (orderedList : List a) :=
      match orderedList with
        | [] => [x]
        | (y :: rest) => if x <= y
            then x :: y :: rest
            else y :: (insert x rest)

#guard insertionSort ([] : List Int) == []
#guard insertionSort [2, 3, 1] == [1, 2, 3]
#guard insertionSort [2, 3, 1] == [1, 2, 3]
#guard insertionSort [2, 3, 1, 1] == [1, 1, 2, 3]

def quickSort [BEq a] [LT a] [DecidableRel (α := a) (· < ·)] (xs : List a) : List a :=
  match xs with
    | [] => []
    | (x :: rest) => (quickSort $ smallers x rest)
                      ++ (x :: equals x rest)
                      ++ (quickSort $ greaters x rest)
termination_by xs.length
decreasing_by
  · simp_wf; unfold quickSort.smallers; have h := List.length_filter_le (· < x) rest; omega
  · simp_wf; unfold quickSort.greaters; have h := List.length_filter_le (· > x) rest; omega
where
  smallers (x : a) := List.filter (· < x)
  equals (x : a) := List.filter (· == x)
  greaters (x : a) := List.filter (· > x)

#guard quickSort ([] : List Int) == []
#guard quickSort [2, 3, 1] == [1, 2, 3]
#guard quickSort [2, 3, 1] == [1, 2, 3]
#guard quickSort [2, 1, 3, 1, 2, 4] == [1, 1, 2, 2, 3, 4]
#guard quickSort [1, 1] == [1, 1]

partial def generativeRecursiveFun [BEq a] [LT a] [DecidableRel (α := a) (· < ·)] (problem : List a) : List a :=
  match problem with
    | [] => solveTrivial []
    | problem => combineSolutions
                  [ generativeRecursiveFun $ generateProblem1 problem
                  , solveProblem problem
                  , generativeRecursiveFun $ generateProblem2 problem
                  ]
where
  solveTrivial (_ : List a) := []
  combineSolutions := List.flatten

  pivot (f : a -> List a -> List a) (xs : List a) :=
    match xs with
      | [] => []
      | (x :: xs) => f x xs

  generateProblem1 := pivot fun x => List.filter (· < x)
  solveProblem := pivot (fun x xs => x :: (List.filter (· == x) xs))
  generateProblem2 := pivot fun x => List.filter (· > x)

#guard generativeRecursiveFun ([] : List Int) == []
#guard generativeRecursiveFun [2, 3, 1] == [1, 2, 3]
#guard generativeRecursiveFun [2, 3, 1] == [1, 2, 3]
#guard generativeRecursiveFun [2, 1, 3, 1, 2, 4] == [1, 1, 2, 2, 3, 4]
#guard generativeRecursiveFun [1] == [1]

end GenerativeRecursion.DesigningAlgorithms
