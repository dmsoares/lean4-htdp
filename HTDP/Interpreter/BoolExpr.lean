namespace Interpreter.BoolExpr

inductive BoolExpr
  | Value (v : Bool)
  | Or (l : BoolExpr) (r : BoolExpr)
  | And (l : BoolExpr) (r : BoolExpr)
  | Not (v : BoolExpr)

def eval (e : BoolExpr): Bool :=
  match e with
  | .Value v => v
  | .Or l r => (eval l) || (eval r)
  | .And l r => (eval l) && (eval r)
  | .Not v => not (eval v)

def simpleExpr : BoolExpr := .Value True
def complexExpr : BoolExpr :=
  .Not $ .Or simpleExpr (.And (.Value True) (.Or (.Value True) (.Value False)))

end Interpreter.BoolExpr
