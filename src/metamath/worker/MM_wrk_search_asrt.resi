open MM_wrk_ctx
open MM_context
open MM_asrt_apply
open Expln_utils_promise

let procName: string

type request
type response

let searchAssertions: (
    ~wrkCtxVer: string,
    ~wrkCtx: mmContext,
    ~wrkSettings: wrkSettings,
    ~typ: option<int>,
    ~pattern: option<expr>,
) => promise<array<applyAssertionResult>>

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
