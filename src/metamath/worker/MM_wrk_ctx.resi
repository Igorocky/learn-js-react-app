open MM_context
open MM_substitution
open MM_parenCounter

let procName: string

type wrkSettings = {
    parens: array<int>, 
    nonSyntaxTypes: array<int>
}

let getWrkCtx: unit => mmContext
let getWrkFrms: unit => Belt_MapString.t<frmSubsData>
let getWrkSettings: unit => wrkSettings
let getWrkParenCnt: unit => parenCnt

type request
type response

let beginWorkerInteractionUsingCtx: (
    ~wrkCtxVer: string,
    ~wrkCtx: mmContext,
    ~wrkSettingsVer: int,
    ~wrkSettings: wrkSettings,
    ~procName: string,
    ~initialRequest: 'req,
    ~onResponse: ( ~resp: 'resp, ~sendToWorker: 'req => unit, ~endWorkerInteraction: unit => unit, ) => unit,
    ~enableTrace: bool=?,
    ()
) => unit

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
