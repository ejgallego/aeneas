module
import RustSourceProvenance
meta import Aeneas.RustSource

open Lean Elab Command Aeneas.RustSource

meta section

run_cmd do
  let env ← getEnv
  let check (declName : Name) (expected : Provenance) : CommandElabM Unit := do
    let actual := find? env declName
    unless actual == some expected do
      throwError "Unexpected Rust provenance for `{declName}`: {repr actual}"

  let path := "tests/src/rust_source_provenance.rs"
  let span : Span := ⟨path, 7, 0, 16, 1, .localPath⟩
  check ``rust_source_provenance.target_value
    ⟨"rust_source_provenance::target_value", span, none⟩
  check ``rust_source_provenance.target_value.«x86_64-apple-darwin»
    ⟨"rust_source_provenance::target_value::x86_64-apple-darwin", span, none⟩
  check ``rust_source_provenance.target_value.«aarch64-apple-darwin»
    ⟨"rust_source_provenance::target_value::aarch64-apple-darwin", span, none⟩

  check ``rust_source_provenance.from_macro
    ⟨"rust_source_provenance::from_macro", ⟨path, 25, 0, 25, 12, .localPath⟩,
      some ⟨path, 21, 8, 21, 30, .localPath⟩⟩

  check ``rust_source_provenance.is_even
    ⟨"rust_source_provenance::is_even", ⟨path, 51, 0, 57, 1, .localPath⟩, none⟩
  check ``rust_source_provenance.is_odd
    ⟨"rust_source_provenance::is_odd", ⟨path, 59, 0, 65, 1, .localPath⟩, none⟩

  let defaultName := "rust_source_provenance::WithDefault::value"
  let defaultSpan : Span := ⟨path, 28, 4, 34, 5, .localPath⟩
  check ``rust_source_provenance.WithDefault.value.default ⟨defaultName, defaultSpan, none⟩
  check ``rust_source_provenance.WithDefault.value.«x86_64-apple-darwin»
    ⟨defaultName ++ "::x86_64-apple-darwin", defaultSpan, none⟩
  check ``rust_source_provenance.WithDefault.value.«aarch64-apple-darwin»
    ⟨defaultName ++ "::aarch64-apple-darwin", defaultSpan, none⟩
  check ``rust_source_provenance.OverridesDefault.Insts.Rust_source_provenanceWithDefault.value
    ⟨"rust_source_provenance::{impl rust_source_provenance::WithDefault for rust_source_provenance::OverridesDefault}::value",
      ⟨path, 42, 4, 44, 5, .localPath⟩, none⟩
  for inherited in #[
      ``rust_source_provenance.UsesDefault.Insts.Rust_source_provenanceWithDefault.value,
      ``rust_source_provenance.WithDefaultUsesDefault.value.«x86_64-apple-darwin»,
      ``rust_source_provenance.WithDefaultUsesDefault.value.«aarch64-apple-darwin»] do
    unless (find? env inherited).isNone do
      throwError "Unexpected Rust provenance for copied default method `{inherited}`"

  unless (find? env ``Nat.succ).isNone do
    throwError "Unexpected Rust provenance for `Nat.succ`"

end
