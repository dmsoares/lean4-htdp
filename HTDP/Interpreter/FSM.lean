namespace Interpreter.FSM

inductive State where
  | Red
  | Yellow
  | Green
  deriving Repr, BEq

inductive Action where
  | Action (state : State) (next : State)
  deriving Repr, BEq

def getNextState (action : Action) :=
  let (.Action _state next) := action
  next

inductive FSM where
  | Machine (initial: State) (actions : List Action)
  deriving Repr, BEq

def exampleA : FSM :=
  .Machine .Red [
    .Action .Red .Green,
    .Action .Yellow .Red,
    .Action .Green .Yellow
  ]

def run (fsm : FSM) : State :=
  let (.Machine state actions) := fsm
  go state actions 10
  where
    go state actions : Nat -> State
      | 0 => state
      | gas + 1 =>
        if gas < 0 then state else
          match findNextState state actions with
            | some nextState => go nextState actions gas
            | none => state
    findNextState (state: State) (actions : List Action) :=
      getNextState <$> findAction state actions
    findAction (state: State) (actions : List Action) :=
      actions.find? $ fun (.Action curr _next) => state == curr

#guard  [.Green , .Red, .Yellow].elem (run exampleA)

-- vv Parser logic vv

inductive Token where
  | lt
  | gt
  | slash
  | eq
  | ident (s : String)
  | str (s : String)
  deriving Repr, BEq

/-- Read a double-quoted string body until the closing `"`. -/
private def consumeStr : List Char → String → String × List Char
  | [], buf => (buf, [])
  | '"' :: rest, buf => (buf, rest)
  | c :: rest, buf => consumeStr rest (buf.push c)

private partial def tokenizeAux (cs : List Char) (acc : List Token) (buf : String) : List Token :=
  let flush (acc : List Token) (buf : String) : List Token :=
    if buf.isEmpty then acc else acc ++ [.ident buf]
  match cs with
  | [] => flush acc buf
  | '<' :: rest => tokenizeAux rest (flush acc buf ++ [.lt]) ""
  | '>' :: rest => tokenizeAux rest (flush acc buf ++ [.gt]) ""
  | '/' :: rest => tokenizeAux rest (flush acc buf ++ [.slash]) ""
  | '=' :: rest => tokenizeAux rest (flush acc buf ++ [.eq]) ""
  | '"' :: rest =>
      let (content, rest') := consumeStr rest ""
      tokenizeAux rest' (flush acc buf ++ [.str content]) ""
  | c :: rest =>
      if c.isWhitespace then tokenizeAux rest (flush acc buf) ""
      else tokenizeAux rest acc (buf.push c)

/-- Tokenize an XML-like FSM source string into a flat list of tokens. -/
def tokenize (s : String) : List Token := tokenizeAux s.toList [] ""

/-- Map a state-attribute value to a `State`. Unknown names yield `none`. -/
def parseState (s : String) : Option State :=
  match s with
  | "red" => some .Red
  | "green" => some .Green
  | "yellow" => some .Yellow
  | _ => none

/-- Parse one `name="value"` attribute pair. -/
def parseAttr : List Token → Option ((String × String) × List Token)
  | .ident k :: .eq :: .str v :: rest => some ((k, v), rest)
  | _ => none

/-- Find the value of attribute `k` among two parsed attributes. -/
private def attrLookup (k : String) (a b : String × String) : Option String :=
  if a.1 == k then some a.2
  else if b.1 == k then some b.2
  else none

/-- Parse one self-closing `<action state="X" next="Y" />` element. -/
def parseAction (tokens : List Token) : Option (Action × List Token) := do
  match tokens with
  | .lt :: .ident "action" :: rest =>
      let (a1, rest) ← parseAttr rest
      let (a2, rest) ← parseAttr rest
      let stateStr ← attrLookup "state" a1 a2
      let nextStr ← attrLookup "next" a1 a2
      let st ← parseState stateStr
      let nx ← parseState nextStr
      match rest with
      | .slash :: .gt :: rest' => some (.Action st nx, rest')
      | _ => none
  | _ => none

/-- Parse zero or more action elements; `fuel` bounds iteration. -/
def parseActions (fuel : Nat) (tokens : List Token) : List Action × List Token :=
  match fuel with
  | 0 => ([], tokens)
  | n + 1 =>
    match parseAction tokens with
    | some (a, rest) =>
        let (as, rest') := parseActions n rest
        (a :: as, rest')
    | none => ([], tokens)

/-- Parse a complete `<machine initial="X"> actions… </machine>` element. -/
def parseMachine (tokens : List Token) : Option FSM := do
  match tokens with
  | .lt :: .ident "machine" :: .ident "initial" :: .eq :: .str initStr :: .gt :: rest =>
      let st ← parseState initStr
      let (actions, rest') := parseActions rest.length rest
      match rest' with
      | .lt :: .slash :: .ident "machine" :: .gt :: _ => some (.Machine st actions)
      | _ => none
  | _ => none

/-- Top-level parser: tokenize a string and parse a machine. -/
def parse (s : String) : Option FSM := parseMachine (tokenize s)

/-- The example source from the task. -/
def exampleSource : String :=
  "<machine initial=\"red\">
     <action state=\"red\"    next=\"green\" />
     <action state=\"green\"  next=\"yellow\" />
     <action state=\"yellow\" next=\"red\" />
   </machine>"

-- Tests: state lookup
#guard parseState "red" == some .Red
#guard parseState "green" == some .Green
#guard parseState "yellow" == some .Yellow
#guard parseState "blue" == none

-- Tests: tokenizer
#guard tokenize "<a/>" == [.lt, .ident "a", .slash, .gt]
#guard tokenize "x=\"y\"" == [.ident "x", .eq, .str "y"]
#guard tokenize "  <  >  " == [.lt, .gt]

-- Tests: single-action machine
#guard parse "<machine initial=\"red\"><action state=\"red\" next=\"green\"/></machine>"
  == some (.Machine .Red [.Action .Red .Green])

-- Tests: the full example
#guard parse exampleSource == some (.Machine .Red [
  .Action .Red .Green,
  .Action .Green .Yellow,
  .Action .Yellow .Red
])

-- Tests: failure cases
#guard parse "<machine initial=\"blue\"></machine>" == none
#guard parse "<machine>" == none
#guard parse "<machine initial=\"red\"><action state=\"red\" next=\"purple\"/></machine>" == none

end Interpreter.FSM
