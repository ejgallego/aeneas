import Lean.Data.Lsp.InitShutdown
import Lean.Data.Lsp.Internal
import Lean.Data.Lsp.Ipc

open Lean Lsp

private def referenceRange (text leadingText reference : String) : IO Range := do
  let [preceding, _] := text.splitOn (leadingText ++ reference)
    | throw <| IO.userError s!"Expected one reference to `{reference}` after `{leadingText}`"
  let map := FileMap.ofString text
  let first := preceding.utf8ByteSize + leadingText.utf8ByteSize
  return ⟨map.utf8PosToLspPos ⟨first⟩, map.utf8PosToLspPos ⟨first + reference.utf8ByteSize⟩⟩

private def definitionAt (id : Nat) (uri : DocumentUri) (origin : Range) :
    Ipc.IpcM LocationLink := do
  Ipc.writeRequest ⟨id, "textDocument/definition", {
    textDocument := ⟨uri⟩
    position := { origin.start with character := origin.start.character + 1 }
    : TextDocumentPositionParams }⟩
  let reply ← Ipc.readResponseAs id (Array LocationLink)
  let #[link] := reply.result
    | throw <| IO.userError s!"Expected one definition link, got {(toJson reply.result).compress}"
  return link

private def withLeanServer (test : Ipc.IpcM Unit) : IO Unit := do
  let child ← IO.Process.spawn {
    toStdioConfig := Ipc.ipcStdioConfig
    cmd := "lean"
    args := #["--server", "-DstderrAsMessages=false"]
    -- On POSIX, this lets cleanup terminate the server and its file workers together.
    setsid := true }
  let exitCode ← try
    let deadline := (← IO.monoMsNow) + 60_000
    let pause : IO Unit := do
      if (← IO.monoMsNow) ≥ deadline then
        throw <| IO.userError "Lean navigation test did not finish within 60 seconds"
      IO.sleep 20
    let result ← IO.asTask (test.run child) (prio := .dedicated)
    while !(← IO.hasFinished result) do
      pause
    IO.ofExcept (← IO.wait result)
    waitForExit child pause
  catch error =>
    try
      child.kill
    finally
      discard child.wait
    throw error
  unless exitCode == 0 do
    throw <| IO.userError s!"Lean server exited with code {exitCode}"
where
  waitForExit (child : IO.Process.Child Ipc.ipcStdioConfig) (pause : IO Unit) : IO UInt32 := do
    repeat
      if let some code ← child.tryWait then return code
      pause
    unreachable!

def main : IO Unit := do
  let leanPath ← IO.FS.realPath "scripts/RustSourceNavigationFixture.lean"
  let rustPath ← IO.FS.realPath "../src/rust_source_provenance.rs"
  let generatedPath ← IO.FS.realPath "RustSourceProvenance.lean"
  let leanUri := System.Uri.pathToUri leanPath
  let rustUri := System.Uri.pathToUri rustPath
  let text ← IO.FS.readFile leanPath
  let x86Name := "target_value.«x86_64-apple-darwin»"
  let x86 := "rust_source_provenance." ++ x86Name
  let targetRange : Range := ⟨⟨6, 0⟩, ⟨15, 1⟩⟩
  let targetName : Range := ⟨⟨6, 7⟩, ⟨6, 19⟩⟩
  let cases : Array (String × Range × Range) := #[
    (x86, targetRange, targetName),
    ("rust_source_provenance.target_value.«aarch64-apple-darwin»", targetRange, targetName),
    ("rust_source_provenance.from_macro", ⟨⟨20, 8⟩, ⟨20, 30⟩⟩, ⟨⟨20, 15⟩, ⟨20, 25⟩⟩)]

  withLeanServer do
    Ipc.writeRequest ⟨0, "initialize", { capabilities := {} : InitializeParams }⟩
    discard <| Ipc.readResponseAs 0 InitializeResult
    Ipc.writeNotification ⟨"initialized", InitializedParams.mk⟩
    Ipc.writeNotification ⟨"textDocument/didOpen", {
      textDocument := { uri := leanUri, languageId := "lean", version := 1, text }
      : DidOpenTextDocumentParams }⟩
    if let some diags ← Ipc.collectDiagnostics 1 leanUri 1 then
      let errors := diags.param.diagnostics.filter (·.severity? == some .error)
      unless errors.isEmpty do
        throw <| IO.userError s!"Could not elaborate Rust source fixture: {toJson errors}"

    for ((reference, range, selection), i) in cases.zipIdx do
      let origin ← referenceRange text "```rustSource " reference
      let link ← definitionAt (i + 2) leanUri origin
      unless link.originSelectionRange? == some origin && link.targetUri == rustUri &&
          link.targetRange == range && link.targetSelectionRange == selection do
        throw <| IO.userError s!"Unexpected Rust definition for `{reference}`: {toJson link}"

    let leanReference ← referenceRange text "    " x86
    let leanTarget ← referenceRange (← IO.FS.readFile generatedPath) "def " x86Name
    let link ← definitionAt (cases.size + 2) leanUri leanReference
    unless link.targetUri == System.Uri.pathToUri generatedPath &&
        link.targetSelectionRange == leanTarget do
      throw <| IO.userError s!"Expected the generated Lean definition: {toJson link}"

    Ipc.shutdown (cases.size + 3)

  IO.println "Rust source navigation and ordinary Lean go-to-definition passed"
