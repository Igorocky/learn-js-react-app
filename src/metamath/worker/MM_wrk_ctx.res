open MM_context
open MM_wrk_client
open MM_parenCounter
open MM_substitution

let procName = "MM_wrk_ctx"

type wrkSettings = {
    parens: array<int>,
    nonSyntaxTypes: array<int>,
}

let wrkCtxVer = ref("")
let wrkCtx = ref(createContext(()))
let wrkFrms = ref(Belt_MapString.empty)
let wrkSettingsVer = ref(-1)
let wrkSettings = ref({parens:[], nonSyntaxTypes:[]})
let wrkParenCnt = ref(parenCntMake(wrkSettings.contents.parens))

let getWrkCtx = () => wrkCtx.contents
let getWrkFrms = () => wrkFrms.contents
let getWrkSettings = () => wrkSettings.contents
let getWrkParenCnt = () => wrkParenCnt.contents

type request = 
    | CheckVersionsAreUpToDate({wrkCtxVer:string, wrkSettingsVer:int})
    | SetWrkCtx({ver:string, ctx:mmContext})
    | SetWrkSettings({ver:int, settings:wrkSettings})

type response =
    | GetWrkCtx({ver:string})
    | GetWrkSettings({ver:int})
    | Ok

let thisProcName = procName

let beginWorkerInteractionUsingCtx = (
    ~wrkCtxVer:string,
    ~wrkCtx:mmContext,
    ~wrkSettingsVer:int,
    ~wrkSettings:wrkSettings,
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false,
    ()
) => {
    beginWorkerInteraction(
        ~procName = thisProcName,
        ~initialRequest = CheckVersionsAreUpToDate({wrkCtxVer, wrkSettingsVer}), 
        ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
            switch resp {
                | GetWrkCtx({ver:verRequested}) => {
                    if (verRequested == wrkCtxVer) {
                        sendToWorker(SetWrkCtx({ver:wrkCtxVer, ctx:wrkCtx}))
                        sendToWorker(CheckVersionsAreUpToDate({wrkCtxVer, wrkSettingsVer}))
                    }
                }
                | GetWrkSettings({ver:verRequested}) => {
                    if (verRequested == wrkSettingsVer) {
                        sendToWorker(SetWrkSettings({ver:wrkSettingsVer, settings:wrkSettings}))
                        sendToWorker(CheckVersionsAreUpToDate({wrkCtxVer, wrkSettingsVer}))
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
        | CheckVersionsAreUpToDate({wrkCtxVer:newWrkCtxVer, wrkSettingsVer:newWrkSettingsVer}) => {
            let everythingIsUpToDate = ref(true)
            if (wrkCtxVer.contents != newWrkCtxVer) {
                everythingIsUpToDate.contents = false
                sendToClient(GetWrkCtx({ver:newWrkCtxVer}))
            }
            if (wrkSettingsVer.contents != newWrkSettingsVer) {
                everythingIsUpToDate.contents = false
                sendToClient(GetWrkSettings({ver:newWrkSettingsVer}))
            }
            if (everythingIsUpToDate.contents) {
                sendToClient(Ok)
            }
        }
        | SetWrkCtx({ver, ctx}) => {
            wrkCtxVer.contents = ver
            wrkCtx.contents = ctx
            wrkFrms.contents = prepareFrmSubsData(ctx)
        }
        | SetWrkSettings({ver, settings:newWrkSettings}) => {
            wrkSettingsVer.contents = ver
            wrkSettings.contents = newWrkSettings
            wrkParenCnt.contents = parenCntMake(newWrkSettings.parens)
        }
    }
}