open MM_wrk_api_2

@val external sendToClient: workerResponse => unit = "postMessage"

let processRequest: workerRequest => unit = req => {
    if (req.procName == MM_wrk_TextLength.procName) {
        MM_wrk_TextLength.processOnWorkerSide(
            ~req = deserialize(req.body), 
            ~sendToClient = payload=>sendToClient({senderId:req.senderId,body:serialize(payload)}),
        )
    }
}
