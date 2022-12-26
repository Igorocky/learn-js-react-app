open MM_context
open MM_wrk_client
open MM_parenCounter
open MM_substitution

let procName = "MM_wrk_ctx"

type wrkSettings = {
    parens: array<int>,
}

let wrkCtxVer = ref("")
let wrkCtx = ref(createContext(()))
let wrkSettings = ref({parens:[]})
let wrkFrms = ref(Belt_MapString.empty)
let wrkParenCnt = ref(parenCntMake(wrkSettings.contents.parens))

let getWrkCtx = () => wrkCtx.contents
let getWrkFrms = () => wrkFrms.contents
let getWrkSettings = () => wrkSettings.contents
let getWrkParenCnt = () => wrkParenCnt.contents

type request = 
    | CheckVersionsAreUpToDate({wrkCtxVer:string})
    | SetWrkCtx({ver:string, ctx:mmContext, settings:wrkSettings})

type response =
    | GetWrkCtx({ver:string})
    | Ok

let thisProcName = procName

let beginWorkerInteractionUsingCtx = (
    ~wrkCtxVer:string,
    ~wrkCtx:mmContext,
    ~wrkSettings:wrkSettings,
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false,
    ()
) => {
    beginWorkerInteraction(
        ~procName = thisProcName,
        ~initialRequest = CheckVersionsAreUpToDate({wrkCtxVer:wrkCtxVer}), 
        ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
            switch resp {
                | GetWrkCtx({ver:verRequested}) => {
                    if (verRequested == wrkCtxVer) {
                        sendToWorker(SetWrkCtx({ver:wrkCtxVer, ctx:wrkCtx, settings:wrkSettings}))
                        sendToWorker(CheckVersionsAreUpToDate({wrkCtxVer:wrkCtxVer}))
                    }
                }
                | Ok => {
                    endWorkerInteraction()
                    beginWorkerInteraction(~procName, ~initialRequest, ~onResponse, ~enableTrace, ())
                }
            }
        },
        ~enableTrace,
        ()
    )
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | CheckVersionsAreUpToDate({wrkCtxVer:newWrkCtxVer}) => {
            if (wrkCtxVer.contents != newWrkCtxVer) {
                sendToClient(GetWrkCtx({ver:newWrkCtxVer}))
            } else {
                sendToClient(Ok)
            }
        }
        | SetWrkCtx({ver, ctx, settings:newWrkSettings}) => {
            wrkCtxVer.contents = ver
            wrkCtx.contents = ctx
            wrkFrms.contents = prepareFrmSubsData(ctx)
            wrkSettings.contents = newWrkSettings
            wrkParenCnt.contents = parenCntMake(newWrkSettings.parens)
        }
    }
}