open MM_context
open MM_substitution
open MM_parenCounter
open MM_wrk_editor

let procName: string

let getWrkPreData: unit => wrkPrecalcData

type request
type response

let beginWorkerInteractionUsingCtx: (
    ~wrkPreData: wrkPrecalcData,
    ~procName: string,
    ~initialRequest: 'req,
    ~onResponse: ( ~resp: 'resp, ~sendToWorker: 'req => unit, ~endWorkerInteraction: unit => unit, ) => unit,
    ~enableTrace: bool=?,
    ()
) => unit

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
