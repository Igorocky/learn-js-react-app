open MM_wrk_api_2

@val external sendToClient: workerResponse => unit = "postMessage"

let processRequest: workerRequest => unit = req => {
    switch req {
        | {senderId, body:MM_wrk_TextLength(req)} => {
            MM_wrk_TextLength.processOnWorkerSide(
                ~req, 
                ~sendToClient = payload=>sendToClient({senderId,body:MM_wrk_TextLength(payload)}),
            )
        }
    }
}
