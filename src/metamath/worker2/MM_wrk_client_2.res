open MM_wrk_api_2

@val external webworker: {..} = "window.webWorkerInst"
let sendToWorker: workerRequest => unit = req => webworker["postMessage"](. req)

type listenerResp =
    | ContinuePropagation
    | StopPropagation
    | StopPropagationAndUnregister

type listener = {
    id: int,
    callback: workerResponse => listenerResp
}

let nextId = ref(0)

let getNextId = () => {
    nextId.contents = nextId.contents + 1
    nextId.contents - 1
}

let listeners = []

let regWorkerListener = callback => {
    let id = getNextId()
    listeners->Js_array2.push({ id, callback })->ignore
    id
}

let unregWorkerListener = id => {
    let i = ref(0)
    while (i.contents < listeners->Js_array2.length) {
        if (listeners[i.contents].id == id) {
            listeners->Js_array2.removeCountInPlace(~pos=i.contents, ~count=1)->ignore
        } else {
            i.contents = i.contents + 1
        }
    }
}

webworker["onmessage"] = msg => {
    let resp = msg["data"]
    let i = ref(0)
    while (i.contents < listeners->Js_array2.length) {
        let listener = listeners[i.contents]
        switch listener.callback(resp) {
            | ContinuePropagation => i.contents = i.contents + 1
            | StopPropagation => i.contents = listeners->Js_array2.length
            | StopPropagationAndUnregister => {
                unregWorkerListener(listener.id)
                i.contents = listeners->Js_array2.length
            }
        }
    }
}

let beginWorkerInteraction = (
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp,~endWorkerInteraction:unit=>unit)=>unit,
) => {
    let id = ref(-1)
    id.contents = regWorkerListener(resp => {
        if (resp.senderId == id.contents) {
            onResponse(~resp=deserialize(resp.body),~endWorkerInteraction= _=>unregWorkerListener(id.contents))
            StopPropagationAndUnregister
        } else {
            ContinuePropagation
        }
    })
    sendToWorker({senderId:id.contents, procName, body:serialize(initialRequest)})
}
