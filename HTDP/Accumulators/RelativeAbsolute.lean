namespace Accumulators.RelativeAbsolute

def RelativeDistances := List Int
  deriving BEq
def AbsoluteDistances := List Int
  deriving BEq

def relative2absolute : (rds : RelativeDistances) -> AbsoluteDistances :=
  List.reverse ∘ revDistances ∘ List.reverse
    where
      revDistances rds :=
        match rds with
          | [] => []
          | (rd :: rest) => (rd + rest.sum) :: revDistances rest

#eval relative2absolute [50, 40, 70, 30, 30]
#guard relative2absolute [50, 40, 70, 30, 30] == [50, 90, 160, 190, 220]

partial def relative2absolute' (rds : RelativeDistances) : AbsoluteDistances :=
  match rds with
    | [] => []
    | (rd :: rest) => rd :: relative2absolute' (add2first rd rest)
  where
    add2first : (n : Int) -> (rds : RelativeDistances) -> RelativeDistances
      | _, [] => []
      | n, (rd :: rest) => (n + rd) :: rest

#eval relative2absolute' [50, 40, 70, 30, 30]
#guard relative2absolute' [50, 40, 70, 30, 30] == [50, 90, 160, 190, 220]

-- How: given a absolute list of distances, prepending a new relative distance
-- is the same as adding it to every absolute distance on the list.
-- Example:
-- absolute distances := 1, 4, 6, 7
-- new relative distance := 4
-- final list := 4, 5, 8, 10, 11
def relative2absolute'' (rds : RelativeDistances) : AbsoluteDistances :=
  match rds with
    | [] => []
    | (rd :: rest) => rd :: (relative2absolute'' rest).map (· + rd)

#eval relative2absolute'' [50, 40, 70, 30, 30]
#guard relative2absolute'' [50, 40, 70, 30, 30] == [50, 90, 160, 190, 220]
-- [50, 40, 70, 30, 30]
-- 50 :: (r2a [40, 70, 30, 30]).map (· + 50)
-- 50 :: (40 :: (r2a [70, 30, 30]).map (· + 40)).map (· + 50)
-- 50 :: (40 :: (70 :: (r2a [30, 30]).map (· + 70)).map (· + 40)).map (· + 50)
-- 50 :: (40 :: (70 :: (30 :: (r2a [30]).map (· + 30)).map (· + 70)).map (· + 40)).map (· + 50)
-- 50 :: (40 :: (70 :: (30 :: (30 :: (r2a []).map (· + 30)).map (· + 30)).map (· + 70)).map (· + 40)).map (· + 50)
-- 50 :: (40 :: (70 :: (30 :: (30 :: [].map (· + 30)).map (· + 30)).map (· + 70)).map (· + 40)).map (· + 50)
-- 50 :: (40 :: (70 :: (30 :: ([30]).map (· + 30)).map (· + 70)).map (· + 40)).map (· + 50)
-- 50 :: (40 :: (70 :: (30 :: ([60]).map (· + 70)).map (· + 40)).map (· + 50)
-- 50 :: (40 :: (70 :: [100, 130]).map (· + 40)).map (· + 50)
-- 50 :: (40 :: [110, 140, 170]).map (· + 50)
-- 50 :: [90, 160, 190, 220]
-- [50, 90, 160, 190, 220]

def State := Int × AbsoluteDistances

def memoR2A (rds : RelativeDistances) : AbsoluteDistances :=
  let (_, ads) := List.foldl f (0, []) rds
  ads.reverse
  where
    f (state : State) (n : Int) :=
      let (s, ads) := state
      let s := s + n
      (s, s :: ads)

#eval memoR2A [50, 40, 70, 30, 30]
#guard memoR2A [50, 40, 70, 30, 30] == [50, 90, 160, 190, 220]

def memoR2A' (rds : RelativeDistances) : AbsoluteDistances :=
  go 0 rds
  where
    go n rds :=
      match rds with
        | [] => []
        | (rd :: rest) => (n + rd) :: go (n + rd) rest

#eval memoR2A' [50, 40, 70, 30, 30]
#guard memoR2A' [50, 40, 70, 30, 30] == [50, 90, 160, 190, 220]

def memoR2A'' (rds : RelativeDistances) : AbsoluteDistances :=
  (StateT.run' (m := Id) (List.foldlM f [] rds) 0).reverse
  where
    f (ads : AbsoluteDistances) (n : Int) : StateM Int AbsoluteDistances := do
      let acc' <- modifyGet $ λ acc => let acc' := acc + n; (acc', acc')
      pure $ acc' :: ads

#eval memoR2A'' [50, 40, 70, 30, 30]
#guard (memoR2A'' [50, 40, 70, 30, 30]) == [50, 90, 160, 190, 220]


end Accumulators.RelativeAbsolute
