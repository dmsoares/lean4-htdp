import HTDP.Interpreter.NumExpr
import HTDP.Interpreter.BoolExpr
import HTDP.Interpreter.FSM

namespace Interpreter

def printWithTag (t: String) (s: String) : IO Unit := do
  IO.println s!"{t}: {s}"

def run : IO Unit := do
  printWithTag "NumExpr simple" (toString $ NumExpr.eval [] NumExpr.simpleExpr)
  printWithTag "NumExpr complex" (toString $ NumExpr.eval [] NumExpr.complexExpr)
  printWithTag "NumExpr with variable" (toString $ NumExpr.parse "(* x (+ 1 2))" |>.map (NumExpr.eval [("x", .Num 5)]))

  printWithTag "NumExpr parse '(+ 1 2)'" (toString $ (NumExpr.parse "(+ 1 2)").map (NumExpr.eval []))
  printWithTag "NumExpr parse '(* 3 (+ 1 2))'" (toString $ NumExpr.parse "(* 3 (+ 1 2))" |>.map (NumExpr.eval []))

  printWithTag "BoolExpr simple" (toString ∘ BoolExpr.eval $ BoolExpr.simpleExpr)
  printWithTag "BoolExpr complex" (toString ∘ BoolExpr.eval $ BoolExpr.complexExpr)

  printWithTag "FSM parse example" (toString $ reprStr (FSM.parse FSM.exampleSource))
  printWithTag "FSM run example" (toString $ reprStr (FSM.run <$> (FSM.parse FSM.exampleSource)))

end Interpreter
