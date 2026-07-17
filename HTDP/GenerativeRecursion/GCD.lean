import Mathlib.Data.PNat.Basic

namespace GenerativeRecursion.DesigningAlgorithms

def gcdStructural (n : Nat) (m : Nat) : Nat :=
  greatestDivisor $ min n m
where
  greatestDivisor : (i : Nat) -> Nat
    | 0 => 0
    | 1 => 1
    | i + 2 =>
        let i := i + 2
        if (n % i == m % i && m % i == 0)
          then i
          else greatestDivisor (i - 1)

#guard gcdStructural 6 25 == 1
#guard gcdStructural 18 24 == 6
-- #guard gcdStructural 101135853 45014640 = 177

def gcdStructural' (n : PNat) (m : PNat) : PNat :=
  (greatestDivisor $ min n m).toPNat'
where
  greatestDivisor : (i : Nat) -> Nat
    | 0 => 0
    | 1 => 1
    | i + 2 =>
        let i := i + 2
        if (n % i == m % i && m % i == 0)
          then i
          else greatestDivisor (i - 1)

#guard gcdStructural' 6 25 == 1
#guard gcdStructural' 18 24 == 6

def gcd (n : Nat) (m : Nat) : Nat :=
  cleverGcd (max n m) (min n m)
  where
    cleverGcd l s :=
      match s with
        | 0 => l
        | s + 1 =>
          let s := s + 1
          cleverGcd s (l % s)
    termination_by s
    decreasing_by
      · simp_wf; exact Nat.mod_lt l (by omega)

#guard gcd 6 25 == 1
#guard gcd 18 24 == 6
#guard gcd 101135853 45014640 = 177

end GenerativeRecursion.DesigningAlgorithms
