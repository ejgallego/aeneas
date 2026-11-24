module
public import Lean
public meta import Lean

public meta section

namespace Aeneas.RustSource
open Lean

/-- Charon's classification of the source path. A local path can use a
remapped prefix such as `/cargo`, `/rustc`, or `/toolchain`. -/
inductive PathKind where
  | localPath
  | virtualPath
  | notReal
  deriving BEq, Inhabited, Repr

/-- A half-open range in the Rust input compiled by Charon. Lines are one-based
and columns are zero-based rustc display columns. Paths retain Charon's spelling.
Consumers resolve them against the translation inputs and convert columns for
their editor protocol. -/
structure Span where
  path : String
  startLine : Nat
  startColumn : Nat
  endLine : Nat
  endColumn : Nat
  pathKind : PathKind := .localPath
  deriving BEq, Inhabited, Repr

/-- Source provenance of an extracted function. `rustName` is Charon's display
name, which can include target qualifiers. For a macro-generated item, `span`
identifies the invocation and `generatedFrom?` preserves the underlying source
range reported by Charon, when available. -/
structure Provenance where
  rustName : String
  span : Span
  generatedFrom? : Option Span := none
  deriving BEq, Inhabited, Repr

declare_syntax_cat rustSourceSpan
syntax "(" str num ":" num "-" num ":" num (ident)? ")" : rustSourceSpan

/-- Attach Rust provenance to a translated function. Aeneas emits this attribute
when given `-lean-rust-source`. -/
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

/-- Look up Rust provenance by the fully qualified Lean declaration name.
Entries on public declarations are available after importing their module. -/
def find? (env : Environment) (declName : Name) : Option Provenance :=
  rustSource.getParam? env declName

end Aeneas.RustSource
