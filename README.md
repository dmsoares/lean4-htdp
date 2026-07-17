# HtDP in Lean 4

A collection of problems and explorations from *How to Design Programs*
(HtDP), worked through in Lean 4.

This is my first time using Lean 4, so the code here is as much about
learning the language as it is about the exercises. The aim is compound
learning: each chapter builds on the last, revisiting earlier ideas with
new tools and reinforcing them along the way.

## Learning with Claude

I used Claude in a Socratic mode as a study partner — especially for the
parts of Lean that were new to me, like writing termination proofs for
non-structural recursion (`termination_by` / `decreasing_by`). Rather than
handing me finished proofs, it nudged me toward the right measure and let
me work out the argument myself, which made the ideas stick.

## Layout

The exercises are grouped by chapter/topic under `HTDP/`:

- `Accumulators/` — relative/absolute distances, path finding
- `GenerativeRecursion/` — sorting, GCD, graph traversal
- `Interpreter/` — NumExpr, BoolExpr, and FSM interpreters
- `BST.lean` — binary search trees
- `Filesystem.lean` — self-referential directory data

## Building

The project depends on Mathlib. On NixOS, use the provided dev shell so
Mathlib's cache tool can link against OpenSSL:

```sh
nix-shell
lake exe cache get
lake build
```
