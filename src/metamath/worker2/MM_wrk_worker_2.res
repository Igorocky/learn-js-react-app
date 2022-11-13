open MM_wrk_api_2

@val external sendToClient: workerResponse => unit = "postMessage"

type requestProcessor = (~req:workerRequest, ~sendToClient:workerResponse=>unit) => unit
type workerSideTask<'req,'resp> = (~req:'req, ~sendToClient:'resp=>unit) => unit

let toRequestProcessor = (func:workerSideTask<'req,'resp>):requestProcessor => 
        (~req:workerRequest, ~sendToClient:workerResponse=>unit) => func(
            ~req = deserialize(req.body), 
            ~sendToClient = resp=>sendToClient({senderId:req.senderId,body:serialize(resp)}),
        )

//module type WorkerTask = {
    //type request
    //type response
    //let procName: string
    //let processOnWorkerSide:workerSideTask<request,response>
//}

//let createRequestProcessors = (modules:array<module(WorkerTask)>):Belt_MapString.t<requestProcessor> => {
    //modules
        //->Js_array2.map((module modul) => ("", toRequestProcessor((module(modul)).processOnWorkerSide)))
        //->Belt_MapString.fromArray
//}

let processors: Belt_MapString.t<requestProcessor> = Belt_MapString.fromArray([
    (
        MM_wrk_TextLength.procName, 
        toRequestProcessor(MM_wrk_TextLength.processOnWorkerSide)
    ),
    (
        MM_wrk_ParseMmFile.procName, 
        toRequestProcessor(MM_wrk_ParseMmFile.processOnWorkerSide)
    ),
    (
        MM_wrk_LoadCtx.procName, 
        toRequestProcessor(MM_wrk_LoadCtx.processOnWorkerSide)
    ),
])

let processRequest: workerRequest => unit = req => {
    processors->Belt_MapString.get(req.procName)->Belt_Option.forEach(processor => processor(~req=req, ~sendToClient))
}
