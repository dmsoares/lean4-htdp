namespace Filesystem

inductive DirV1 : Type where
  | None
  | File (head : String) (tail : DirV1)
  | Dir (head : DirV1) (tail : DirV1)

-- def directory : DirV1 :=
--     .File "read!" (
--     .Dir (.File "part1" (.File "part2" (.File "part3" .None))) (
--     .Dir (.Dir (.File "hang" (.File "draw" .None)) (.Dir (.File "read!" .None) .None))))

def read! := "read!"
def part1 := "part1"
def part2 := "part2"
def part3 := "part3"
def hang := "hang"
def draw := "draw"

def a : DirV1 :=
  .File read! .None

def b : DirV1 :=
  .File part1 (.File part2 (.File part3 .None))

def c : DirV1 :=
  .File hang (.File draw .None)

def d : DirV1 :=
  .File read! .None

def Text : DirV1 :=
  .Dir a .None

def Code : DirV1 :=
  .Dir c .None

def Docs : DirV1 :=
  .Dir d .None

def Libs : DirV1 :=
  .Dir Code Docs

def TS : DirV1 :=
  .Dir Text (.File read! (.Dir a Libs))

end Filesystem
