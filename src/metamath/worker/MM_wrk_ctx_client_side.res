open MM_context
open MM_wrk_ctx
open MM_wrk_client


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
        ~procName = MM_wrk_ctx.procName,
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