open MM_wrk_api

@val external sendToClient: workerResponse => unit = "postMessage"

type requestProcessor = (~req:workerRequest, ~sendToClient:workerResponse=>unit) => unit
type workerFunc<'req,'resp> = (~req:'req, ~sendToClient:'resp=>unit) => unit

let makeRequestProcessor = (func:workerFunc<'req,'resp>):requestProcessor => 
        (~req:workerRequest, ~sendToClient:workerResponse=>unit) => func(
            ~req = deserialize(req.body), 
            ~sendToClient = resp=>sendToClient({clientId:req.clientId, body:serialize(resp)}),
        )

let processors: Belt_MapString.t<requestProcessor> = Belt_MapString.fromArray([
    (
        MM_wrk_ParseMmFile.procName, 
        makeRequestProcessor(MM_wrk_ParseMmFile.processOnWorkerSide)
    ),
    (
        MM_wrk_LoadCtx.procName, 
        makeRequestProcessor(MM_wrk_LoadCtx.processOnWorkerSide)
    ),
    (
        MM_wrk_FindParens.procName, 
        makeRequestProcessor(MM_wrk_FindParens.processOnWorkerSide)
    ),
    (
        MM_wrk_ctx.procName, 
        makeRequestProcessor(MM_wrk_ctx.processOnWorkerSide)
    ),
])

let processRequest: workerRequest => unit = req => {
//    Js.Console.log(`[clientId=${req.clientId->Belt_Int.toString}] worker receved a request, procName = ${req.procName}`)
    processors->Belt_MapString.get(req.procName)->Belt_Option.forEach(processor => processor(~req, ~sendToClient=resp=>{
//        Js.Console.log(`[clientId=${resp.clientId->Belt_Int.toString}] worker is sending a response`)
        sendToClient(resp)
    }))
}
