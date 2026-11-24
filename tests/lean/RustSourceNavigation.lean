module
public meta import Lean.Server.FileWorker.RequestHandling

public meta section

namespace RustSourceNavigation
open Lean Elab Term Lsp Server

private structure RustSourceTarget where
  uri : DocumentUri
  range : Lsp.Range
  selection : Lsp.Range
  deriving TypeName

private def rustDefinitionAt (params : TextDocumentPositionParams) :
    RequestM (RequestTask (Option LeanLocationLink)) := do
  let doc ← RequestM.readDoc
  let pos := doc.meta.text.lspPosToUtf8Pos params.position
  RequestM.withWaitFindSnap doc (fun snap => pos ≤ snap.endPos) (notFoundX := pure none) fun snap => do
    let candidate := snap.infoTree.findInfo? fun
      | .ofCustomInfo info => (info.value.get? RustSourceTarget).isSome &&
          info.stx.getRange?.any (fun r => r.start ≤ pos && pos < r.stop)
      | _ => false
    let some (.ofCustomInfo info) := candidate | return none
    let some target := info.value.get? RustSourceTarget | return none
    let some range := info.stx.getRange? | return none
    return some {
      originSelectionRange? := some (range.toLspRange doc.meta.text)
      targetUri := target.uri
      targetRange := target.range
      targetSelectionRange := target.selection
      ident? := none
      isDefault := false }

-- The fixture wraps Lean's definition handler and delegates requests outside its Rust references.
initialize
  chainLspRequestHandler "textDocument/definition" TextDocumentPositionParams
    (Array LeanLocationLink) fun params previous => do
      RequestM.bindRequestTaskCostly (← rustDefinitionAt params) fun
        | none => pure previous
        | some link => return .pure #[link]

/-- Attach the fixture's Rust definition link to its Verso argument. -/
def record (stx : Syntax) (uri : DocumentUri) (range selection : Lsp.Range) : TermElabM Unit :=
  pushInfoLeaf (.ofCustomInfo { stx, value := Dynamic.mk (⟨uri, range, selection⟩ : RustSourceTarget) })

end RustSourceNavigation
