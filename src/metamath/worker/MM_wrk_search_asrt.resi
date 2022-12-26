open MM_wrk_ctx
open MM_context
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_editor

let procName: string

type request
type response

let searchAssertions: (
    ~wrkPreData:wrkPrecalcData,
    ~typ: option<int>,
    ~pattern: option<expr>,
) => promise<array<applyAssertionResult>>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
