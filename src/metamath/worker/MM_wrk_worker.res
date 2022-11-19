open MM_wrk_api

@val external sendToClient: workerResponse => unit = "postMessage"

type requestProcessor = (~req:workerRequest, ~sendToClient:workerResponse=>unit) => unit
type workerSideTask<'req,'resp> = (~req:'req, ~sendToClient:'resp=>unit) => unit

let toRequestProcessor = (func:workerSideTask<'req,'resp>):requestProcessor => 
        (~req:workerRequest, ~sendToClient:workerResponse=>unit) => func(
            ~req = deserialize(req.body), 
            ~sendToClient = resp=>sendToClient({senderId:req.senderId,body:serialize(resp)}),
        )

let processors: Belt_MapString.t<requestProcessor> = Belt_MapString.fromArray([
    (
        MM_wrk_ParseMmFile.procName, 
        toRequestProcessor(MM_wrk_ParseMmFile.processOnWorkerSide)
    ),
    (
        MM_wrk_LoadCtx.procName, 
        toRequestProcessor(MM_wrk_LoadCtx.processOnWorkerSide)
    ),
    (
        MM_wrk_FindParens.procName, 
        toRequestProcessor(MM_wrk_FindParens.processOnWorkerSide)
    ),
])

let processRequest: workerRequest => unit = req => {
//    Js.Console.log(`[senderId=${req.senderId->Belt_Int.toString}] worker receved a request, procName = ${req.procName}`)
    processors->Belt_MapString.get(req.procName)->Belt_Option.forEach(processor => processor(~req=req, ~sendToClient=resp=>{
//        Js.Console.log(`[senderId=${resp.senderId->Belt_Int.toString}] worker is sending a response`)
        sendToClient(resp)
    }))
}
