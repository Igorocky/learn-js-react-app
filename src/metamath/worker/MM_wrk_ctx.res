open MM_context
open MM_wrk_client

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

let thisProcName = procName

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
        ~procName = thisProcName,
        ~initialRequest = CheckVersionsAreUpToDate({ctxVer, settingsVer}), 
        ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
            switch resp {
                | GetCtx({ver:verRequested}) => {
                    if (verRequested == ctxVer) {
                        sendToWorker(SetCtx({ver:ctxVer, ctx}))
                    }
                }
                | GetSettings({ver:verRequested}) => {
                    if (verRequested == settingsVer) {
                        sendToWorker(SetSettings({ver:settingsVer, settings}))
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