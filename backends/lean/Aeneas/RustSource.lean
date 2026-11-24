module
public import Lean
public meta import Lean

public meta section

namespace Aeneas.RustSource
open Lean

/-- How Charon classified the file name. -/
inductive PathKind where
  /-- A file path, possibly remapped under a prefix like `/cargo`, `/rustc`,
  or `/toolchain`. -/
  | localPath
  /-- A virtual path assigned by the compiler, for example to library sources. -/
  | virtualPath
  /-- A synthetic name used by the compiler, for example for a macro or query. -/
  | notReal
  deriving BEq, Inhabited, Repr

/-- A source range in the Rust input as compiled by Charon, including any
translation patches.

The end position is exclusive. Line numbers start at 1. Columns are rustc
display columns starting at 0. Convert columns to the editor's position
encoding before using a range for navigation.

Aeneas keeps the path spelling and classification supplied by Charon.
Resolve relative local paths from the directory where Charon ran rustc.
Use the source version that was translated. -/
structure Span where
  path : String
  startLine : Nat
  startColumn : Nat
  endLine : Nat
  endColumn : Nat
  pathKind : PathKind := .localPath
  deriving BEq, Inhabited, Repr

/-- The Rust name and source range recorded for a generated Lean function.

`rustName` is Charon's display name and can include a target qualifier.
Multiple generated declarations can point to the same Rust range.

For macro-generated code, `span` points to the macro call and
`generatedFrom?` points to the source used in the expansion, if available. -/
structure Provenance where
  rustName : String
  span : Span
  generatedFrom? : Option Span := none
  deriving BEq, Inhabited, Repr

declare_syntax_cat rustSourceSpan
syntax "(" str num ":" num "-" num ":" num (ident)? ")" : rustSourceSpan

/-- Record a Rust source location on a generated function. Aeneas emits this
attribute when given `-lean-rust-source`. -/
syntax (name := rustSourceAttr) "rust_source" str rustSourceSpan
  (&"from" rustSourceSpan)? : attr

private def elabSpan (stx : TSyntax `rustSourceSpan) : AttrM Span := do
  let `(rustSourceSpan| ($path:str $sl:num:$sc:num-$el:num:$ec:num $[$kind:ident]?)) := stx
    | Elab.throwUnsupportedSyntax
  let kind : PathKind ← match kind.map (·.getId) with
    | none => pure .localPath
    | some `virtual => pure .virtualPath
    | some `not_real => pure .notReal
    | some k => throwErrorAt stx "Unknown Rust source path kind `{k}`"
  return ⟨path.getString, sl.getNat, sc.getNat, el.getNat, ec.getNat, kind⟩

private initialize rustSource : ParametricAttribute Provenance ←
  registerParametricAttribute {
    name := `rustSourceAttr
    descr := "Rust source provenance of a translated function"
    getParam := fun _ stx => withRef stx do
      let `(attr| rust_source $name:str $span $[from $generatedFrom]?) := stx
        | Elab.throwUnsupportedSyntax
      return ⟨name.getString, ← elabSpan span, ← generatedFrom.mapM elabSpan⟩
  }

/-- Find the recorded Rust source location for a fully qualified Lean name.
Import the generated module to look up locations on its public declarations.
The query returns `none` if no location was recorded.

Copied trait defaults are skipped because Charon gives the copy the range
of the enclosing `impl`.

In a Lean module, use `meta import Aeneas.RustSource` to access this query. -/
def find? (env : Environment) (declName : Name) : Option Provenance :=
  rustSource.getParam? env declName

end Aeneas.RustSource
