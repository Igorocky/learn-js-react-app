open MM_wrk_client
open MM_context

let procName = "MM_wrk_ctx"

type settings = {
    parens: array<int>,
    nonSyntaxTypes: array<int>,
}

let wrkCtxVer = ref("")
let wrkCtx = ref(createContext(()))
let settingsVer = ref(-1)
let settings = ref({parens:[], nonSyntaxTypes:[]})

type request = 
    | CheckVersionsAreUpToDate({ctxVer:string, settingsVer:int})
    | SetCtx({ver:string, ctx:mmContext})
    | SetSettings({ver:int, settings:settings})

type response =
    | GetCtx({ver:string})
    | GetSettings({ver:int})
    | Ok

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | CheckVersionsAreUpToDate({ctxVer:newCtxVer, settingsVer:newSettingsVer}) => {
            let updateIsRequired = ref(false)
            if (wrkCtxVer.contents != newCtxVer) {
                updateIsRequired.contents = true
                sendToClient(GetCtx({ver:newCtxVer}))
            }
            if (settingsVer.contents != newSettingsVer) {
                updateIsRequired.contents = true
                sendToClient(GetSettings({ver:newSettingsVer}))
            }
            if (!updateIsRequired.contents) {
                sendToClient(Ok)
            }
        }
        | SetCtx({ver, ctx}) => {
            wrkCtxVer.contents = ver
            wrkCtx.contents = ctx
        }
        | SetSettings({ver, settings:newSettings}) => {
            settingsVer.contents = ver
            settings.contents = newSettings
        }
    }
}

let beginWorkerInteractionUsingCtx = (
    ~ctxVer:string,
    ~ctx:mmContext,
    ~settingsVer:int,
    ~settings:settings,
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
) => {
    beginWorkerInteraction(
        ~procName = "MM_wrk_ctx",
        ~initialRequest = CheckVersionsAreUpToDate({ctxVer, settingsVer}), 
        ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
            switch resp {
                | GetCtx({ver}) => {
                    if (ver == ctxVer) {
                        sendToWorker(SetCtx({ver, ctx}))
                    }
                }
                | GetSettings({ver}) => {
                    if (ver == settingsVer) {
                        sendToWorker(SetSettings({ver, settings}))
                    }
                }
                | Ok => {
                    endWorkerInteraction()
                    beginWorkerInteraction(~procName, ~initialRequest, ~onResponse)
                }
            }
        }
    )
}