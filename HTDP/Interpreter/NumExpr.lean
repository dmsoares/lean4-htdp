namespace Interpreter.NumExpr

inductive Expr where
  | Num : Int -> Expr
  | Symbol : String -> Expr
  | Add : Expr -> Expr -> Expr
  | Mul : Expr -> Expr -> Expr
  | Ap : String -> Expr -> Expr
  | Function : String  -> String -> Expr -> Expr
  deriving Repr, BEq

inductive EnvVal where
  | Num : Int -> EnvVal
  | Function : String  -> String -> Expr -> EnvVal

abbrev Env := List (String × EnvVal)

partial def eval (env : Env) (e : Expr) : Option Int :=
  Id.run ∘ StateT.run' (doEval e) $ env
  where
    doEval (e : Expr) : StateM Env (Option Int) :=
      match e with
      | .Num n => pure n
      | .Symbol name => evalSymbol name
      | .Add l r => add <$> doEval l <*> doEval r
      | .Mul l r => mul <$> doEval l <*> doEval r
      | .Ap name arg => evalAp name arg
      | .Function name arg body => evalFunction name arg body
    add m n := (. + .) <$> m <*> n
    mul m n := (. * .) <$> m <*> n
    evalSymbol name := do
      let mVal <- resolveName name
      pure $ match mVal with
        | some (.Num n) => n
        | _ => none
    evalAp (name : String) (arg : Expr) : StateM Env (Option Int) := do
      let mVal <- resolveName name
      let mx <- doEval arg
      match (mVal, mx) with
        | (some (.Function _ p b), some x) => do
            let currEnv <- StateT.get
            StateT.set $ (p, .Num x) :: currEnv
            doEval b
        | _ => pure none
    resolveName (name : String) : StateM Env (Option EnvVal) := do
      let env <- StateT.get
      let mVal := Prod.snd <$> env.find? (fun (n, _) => n == name)
      pure mVal
    evalFunction name arg body := do
      let currEnv <- StateT.get
      StateT.set $ (name, .Function name arg body) :: currEnv
      pure none

def simpleExpr : Expr := .Num 42
def complexExpr : Expr := .Add simpleExpr (.Mul (.Num 4) (.Add (.Num 3) (.Num 5)))

-- vv Parser logic vv

inductive Token
  | lparen
  | rparen
  | plus
  | star
  | num (n : Int)
  | sym (name : String)
  deriving Repr, BEq

/-- Tokenize a string into a list of tokens for s-expression parsing. -/
def tokenize (s : String) : List Token :=
  let rec go (cs : List Char) (acc : List Token) (buf : String) : List Token :=
    let flush (acc : List Token) (buf : String) : List Token :=
      if buf.isEmpty then acc
      else match buf.toInt? with
        | some n => acc ++ [.num n]
        | none =>  acc ++ [.sym buf]
    match cs with
    | [] => flush acc buf
    | c :: rest =>
      if c == '(' then go rest (flush acc buf ++ [.lparen]) ""
      else if c == ')' then go rest (flush acc buf ++ [.rparen]) ""
      else if c == '+' then go rest (flush acc buf ++ [.plus]) ""
      else if c == '*' then go rest (flush acc buf ++ [.star]) ""
      else if c.isWhitespace then go rest (flush acc buf) ""
      else go rest acc (buf.push c)
  go s.toList [] ""

/-- Parse tokens into an Expr, returning the expr and remaining tokens. -/
private def parseAux (fuel : Nat) (tokens : List Token) : Option (Expr × List Token) :=
  match fuel with
  | 0 => none
  | n + 1 =>
    match tokens with
    | .num v :: rest => some (.Num v, rest)
    | .sym name :: rest => some (.Symbol name, rest)
    | .lparen :: .plus :: rest => do
        let (l, rest) ← parseAux n rest
        let (r, rest) ← parseAux n rest
        match rest with
        | .rparen :: rest => some (.Add l r, rest)
        | _ => none
    | .lparen :: .star :: rest => do
        let (l, rest) ← parseAux n rest
        let (r, rest) ← parseAux n rest
        match rest with
        | .rparen :: rest => some (.Mul l r, rest)
        | _ => none
    | _ => none

/-- Parse an s-expression string into an Expr. -/
def parse (s : String) : Option Expr := do
  let tokens := tokenize s
  let (expr, _) ← parseAux tokens.length tokens
  pure expr

-- Tests: parsing variables (sym)
#guard tokenize "x" == [.sym "x"]
#guard parse "x" == some (.Symbol "x")
#guard parse "(+ x 1)" == some (.Add (.Symbol "x") (.Num 1))

-- Tests: eval resolves symbols against the given environment
#guard eval [("x", .Num 5)] (.Symbol "x") == some 5
#guard eval [("x", .Num 5)] (.Add (.Symbol "x") (.Num 1)) == some 6
#guard eval [("x", .Num 4), ("y", .Num 6)] (.Mul (.Symbol "x") (.Symbol "y")) == some 24
#guard eval [] (.Symbol "x") == none                  -- unbound variable
#guard eval [] (.Add (.Symbol "x") (.Num 1)) == none  -- missing value propagates
#guard ((parse "(+ x 1)").map (eval [("x", .Num 5)])) == some (some 6)

end  Interpreter.NumExpr
