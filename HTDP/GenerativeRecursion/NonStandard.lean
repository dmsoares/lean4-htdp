namespace GenerativeRecursion.NonStandard

open List

-- bundles chunks of chars into strings of length n
-- def bundle (chars : List Char) (n : Nat) : List String := []

-- def bundle (chars : List Char) (n : Nat) : List String :=
--   match chars with
--     [] => []
--     (c :: rest) => ... c ... (bundle rest n) ...

partial def chunks : List a -> Nat -> List (List a)
  | [], _n => []
  | list, n => (take n list) :: chunks (drop n list) n

partial def bundle (chars : List Char) : Nat -> List String :=
  let implode l := String.ofList <$> l
  implode ∘ chunks chars

#guard chunks ("abcdef".toList) 2 == [['a','b'], ['c', 'd'], ['e', 'f']]
#guard chunks ("abcdefg".toList) 3 == [['a','b', 'c'], ['d', 'e', 'f'], ['g']]
#guard chunks ['a', 'b'] 3 == [['a', 'b']]
#guard chunks ([] : List Nat) 3 == []

#guard (bundle ("abcdef".toList) 2) == ["ab", "cd", "ef"]
#guard (bundle ("abcdefg".toList) 3) == ["abc", "def", "g"]
#guard (bundle ['a', 'b'] 3) == ["ab"]
#guard (bundle [] 3) == []

end GenerativeRecursion.NonStandard
