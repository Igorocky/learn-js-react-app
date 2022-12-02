open MM_wrk_client
open MM_parser
open MM_context
open Expln_React_common
open Expln_React_Mui
open MM_wrk_FindParens
open Modal
open Expln_utils_promise
open MM_id_generator
open MM_parser
open MM_syntax_tree

let procName = "MM_wrk_ctx"

type settings = {
    parens: array<int>,
    nonSyntaxTypes: array<int>,
}

let rootCtxVer = ref(-1)
let rootCtx = ref(createContext(()))
let settingsVer = ref(-1)
let settings = ref({parens:[], nonSyntaxTypes:[]})

type request = 
    | CheckVersionsAreUpToDate({ctxVer:int, settingsVer:int})
    | SetCtx({ver:int, ctx:mmContext})
    | SetSettings({ver:int, settings:settings})

type response =
    | GetCtx({ver:int})
    | GetSettings({ver:int})
    | Ok

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | CheckVersionsAreUpToDate({ctxVer:newCtxVer, settingsVer:newSettingsVer}) => {
            let updateIsRequired = ref(false)
            if (rootCtxVer.contents != newCtxVer) {
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
            rootCtxVer.contents = ver
            rootCtx.contents = ctx
        }
        | SetSettings({ver, settings:newSettings}) => {
            settingsVer.contents = ver
            settings.contents = newSettings
        }
    }
}

let beginWorkerInteractionUsingCtx = (
    ~ctxVer:int,
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