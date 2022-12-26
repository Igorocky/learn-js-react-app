open MM_context
open MM_wrk_client
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_ctx

let procName = "MM_wrk_search_asrt"

type request = 
    | FindAssertions({typ:option<int>, pattern:option<expr>})

type response =
    | SearchResult({found:array<applyAssertionResult>})

let searchAssertions = (
    ~wrkCtxVer:string,
    ~wrkCtx:mmContext,
    ~wrkSettingsVer:int,
    ~wrkSettings:wrkSettings,
    ~typ:option<int>, 
    ~pattern:option<expr>
): promise<array<applyAssertionResult>> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~wrkCtxVer,
            ~wrkCtx,
            ~wrkSettings,
            ~procName,
            ~initialRequest = FindAssertions({typ, pattern}),
            ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
                switch resp {
                    | SearchResult({found}) => {
                        endWorkerInteraction()
                        resolve(found)
                    }
                }
            },
            ~enableTrace=true,
            ()
        )
    })
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({typ, pattern}) => {
            let results = []
            applyAssertions(
                ~maxVar = getWrkCtx()->getNumOfVars - 1,
                ~frms = getWrkFrms(),
                ~isDisjInCtx = getWrkCtx()->isDisj,
                ~statements = [],
                ~parenCnt = getWrkParenCnt(),
                ~frameFilter = frame => {
                    switch typ {
                        | None => true
                        | Some(typ) => frame.asrt[0] == typ
                    }
                },
                ~onMatchFound = res => {
                    results->Js_array2.push(res)->ignore
                    Continue
                },
                ()
            )
            sendToClient(SearchResult({found:results}))
        }
    }
}