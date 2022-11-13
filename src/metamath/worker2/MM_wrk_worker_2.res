open MM_wrk_api_2

@val external sendToClient: workerResponse => unit = "postMessage"

type requestProcessor = (~req:workerRequest, ~sendToClient:workerResponse=>unit) => unit
type processOnWorkerSide<'req,'resp> = (~req:'req, ~sendToClient:'resp=>unit) => unit

let toRequestProcessor = (func:processOnWorkerSide<'req,'resp>):requestProcessor => 
        (~req:workerRequest, ~sendToClient:workerResponse=>unit) => func(
            ~req = deserialize(req.body), 
            ~sendToClient = resp=>sendToClient({senderId:req.senderId,body:serialize(resp)}),
        )

let processors: Belt_MapString.t<requestProcessor> = Belt_MapString.fromArray([
    (
        MM_wrk_TextLength.procName, 
        toRequestProcessor(MM_wrk_TextLength.processOnWorkerSide)
    )
])

let processRequest: workerRequest => unit = req => {
    processors->Belt_MapString.get(req.procName)->Belt_Option.forEach(processor => processor(~req=req, ~sendToClient))
}
