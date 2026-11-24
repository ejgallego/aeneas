module
import RustSourceProvenance
meta import Aeneas.RustSource
meta import RustSourceNavigation

open Lean Elab Command Term Aeneas.RustSource Aeneas.Std

meta section

open Lean.Doc Lean.Lsp

private def fixturePath : String := "tests/src/rust_source_provenance.rs"

@[doc_code_block]
private def rustSource (decl : Ident) (_ : VersoCodeBlock) : DocM (Block ElabInline ElabBlock) := do
  let declName ← liftM (resolveGlobalConstNoOverload decl.raw : TermElabM Name)
  let some provenance := find? (← getEnv) declName
    | throwErrorAt decl "Missing Rust provenance for `{declName}`"
  let span := provenance.generatedFrom?.getD provenance.span
  unless span.pathKind == .localPath && span.path == fixturePath do
    throwErrorAt decl "Unexpected path for Rust fixture: {span.path}"
  let testDir := (System.FilePath.mk (← getFileName)).parent.getD "."
  let sourcePath ← IO.FS.realPath (testDir / ".." / ".." / ".." / span.path)
  let source ← IO.FS.readFile sourcePath
  -- For this ASCII fixture, rustc display columns agree with Lean columns.
  unless source.toList.all (fun c => c.val < 128 && c != '\t') do
    throwError "Expected an ASCII Rust fixture without tab characters"
  let fileMap := FileMap.ofString source
  let start := fileMap.ofPosition ⟨span.startLine, span.startColumn⟩
  let stop := fileMap.ofPosition ⟨span.endLine, span.endColumn⟩
  let rendered := String.Pos.Raw.extract source start stop
  let [beforeName, afterFn] := rendered.splitOn "fn "
    | throwErrorAt decl "Expected a Rust function in the fixture span"
  let nameLength := (afterFn.toList.takeWhile fun c => c.isAlphanum || c == '_').length
  unless nameLength > 0 do throwErrorAt decl "Expected a Rust function name in the fixture span"
  let nameStart := start.byteIdx + beforeName.utf8ByteSize + 3
  let lsp := fileMap.utf8PosToLspPos
  liftM <| RustSourceNavigation.record decl.raw (System.Uri.pathToUri sourcePath)
    ⟨lsp start, lsp stop⟩ ⟨lsp ⟨nameStart⟩, lsp ⟨nameStart + nameLength⟩⟩
  return .code rendered

end

set_option doc.verso true

/--
```rustSource rust_source_provenance.target_value.«x86_64-apple-darwin»
```

```rustSource rust_source_provenance.target_value.«aarch64-apple-darwin»
```

```rustSource rust_source_provenance.from_macro
```
-/
private theorem previewTargetValue :
    rust_source_provenance.target_value.«x86_64-apple-darwin» = .ok (1#u32) := rfl

run_cmd do
  let some (.inr { text := #[.code x86, .code arm, .code expansion], .. }) ←
      findInternalDocString? (← getEnv) ``previewTargetValue
    | throwError "Expected three Rust code blocks in the proof docstring"
  unless x86 == arm && x86.startsWith "pub fn target_value() -> u32 {\n" && x86.endsWith "\n}" do
    throwError "Expected both target variants to show the original Rust function"
  unless expansion == "pub fn from_macro() {}" do
    throwError "Expected the function from the macro expansion"
