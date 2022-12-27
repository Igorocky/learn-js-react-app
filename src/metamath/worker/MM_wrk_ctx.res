open MM_context
open MM_wrk_client
open MM_parenCounter
open MM_wrk_editor

let procName = "MM_wrk_ctx"

let wrkPreData = ref({
    ver: "",
    wrkCtx: createContext(),
    parens: [],
    parenCnt: parenCntMake([]),
    frms: Belt_MapString.empty,
})

let getWrkPreData = () => wrkPreData.contents

type request = 
    | CheckVersion(string)
    | SetWrkPreData(wrkPrecalcData)

type response =
    | GetWrkPreData(string)
    | Ok

let thisProcName = procName

let beginWorkerInteractionUsingCtx = (
    ~wrkPreData: wrkPrecalcData,
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false,
    ()
) => {
    beginWorkerInteraction(
        ~procName = thisProcName,
        ~initialRequest = CheckVersion(wrkPreData.ver), 
        ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
            switch resp {
                | GetWrkPreData(requestedVer) => {
                    if (requestedVer == wrkPreData.ver) {
                        sendToWorker(SetWrkPreData(wrkPreData))
                        sendToWorker(CheckVersion(wrkPreData.ver))
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
        | CheckVersion(latestVer) => {
            if (wrkPreData.contents.ver != latestVer) {
                sendToClient(GetWrkPreData(latestVer))
            } else {
                sendToClient(Ok)
            }
        }
        | SetWrkPreData(newPreData) => {
            wrkPreData.contents = newPreData
        }
    }
}