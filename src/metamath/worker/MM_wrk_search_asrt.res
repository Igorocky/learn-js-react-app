open MM_context
open MM_asrt_apply
open Expln_utils_promise
open MM_wrk_ctx

let procName = "MM_wrk_search_asrt"

type request = 
    | FindAssertions({typ:int, pattern:option<expr>})

type response =
    | OnProgress(float)
    | SearchResult({found:array<applyAssertionResult>})

let searchAssertions = (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~typ:int, 
    ~pattern:option<expr>,
    ~onProgress:float=>unit,
): promise<array<applyAssertionResult>> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~preCtxVer,
            ~preCtx,
            ~parenStr,
            ~varsText,
            ~disjText,
            ~hyps,
            ~procName,
            ~initialRequest = FindAssertions({typ, pattern}),
            ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(pct) => onProgress(pct)
                    | SearchResult({found}) => {
                        endWorkerInteraction()
                        resolve(found)
                    }
                }
            },
            ~enableTrace=false,
            ()
        )
    })
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | FindAssertions({typ, pattern}) => {
            let results = []
            applyAssertions(
                ~maxVar = getWrkCtxExn()->getNumOfVars - 1,
                ~frms = getWrkFrmsExn(),
                ~isDisjInCtx = getWrkCtxExn()->isDisj,
                ~statements = [],
                ~parenCnt = getWrkParenCntExn(),
                ~frameFilter = frame => frame.asrt[0] == typ,
                ~onMatchFound = res => {
                    results->Js_array2.push(res)->ignore
                    Continue
                },
                ~onProgress = pct => sendToClient(OnProgress(pct)),
                ()
            )
            sendToClient(SearchResult({found:results}))
        }
    }
}