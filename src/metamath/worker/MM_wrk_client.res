open MM_wrk_api

let webworker: option<{..}> = %raw("typeof window !== 'undefined' ? window.webWorkerInst : undefined")
let sendToWorker: workerRequest => unit = req => {
//    Js.Console.log(`[senderId=${req.senderId->Belt_Int.toString}] client is sending a request, procName = ${req.procName}`)
    webworker->Belt_Option.forEach(webworker => webworker["postMessage"](. req))
}

type client = {
    id: int,
    callback: serialized => unit
}

let nextClientId = ref(0)

let getNextClientId = () => {
    nextClientId.contents = nextClientId.contents + 1
    nextClientId.contents - 1
}

let clients = []

let regClient = callback => {
    let id = getNextClientId()
    clients->Js_array2.push({ id, callback })->ignore
    id
}

let unregClient = id => {
    let i = ref(0)
    while (i.contents < clients->Js_array2.length) {
        if (clients[i.contents].id == id) {
            clients->Js_array2.removeCountInPlace(~pos=i.contents, ~count=1)->ignore
        } else {
            i.contents = i.contents + 1
        }
    }
}

webworker->Belt_Option.forEach(webworker => {
    webworker["onmessage"]= msg => {
        let resp:workerResponse = msg["data"]
        let i = ref(0)
        clients->Expln_utils_common.arrForEach(client => {
            if (client.id == resp.clientId) {
//                Js.Console.log(`[senderId=${resp.clientId->Belt_Int.toString}] client received a response`)
                client.callback(resp.body)
                Some(())
            } else {
                None
            }
        })->ignore
    }
})

let beginWorkerInteraction = (
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
) => {
    let id = ref(-1)
    let localSendToWorker = ref(_=>())
    id.contents = regClient(respBody => {
        onResponse(
            ~resp=deserialize(respBody),
            ~sendToWorker=localSendToWorker.contents,
            ~endWorkerInteraction= _=>unregClient(id.contents)
        )
    })
    localSendToWorker.contents = req => sendToWorker({clientId:id.contents, procName, body:serialize(req)})
    sendToWorker({clientId:id.contents, procName, body:serialize(initialRequest)})
}
