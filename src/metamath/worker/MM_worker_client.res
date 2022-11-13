open MM_worker_api

@val external webworker: {..} = "window.webWorkerInst"
let sendToWorker: beRequest => unit = req => webworker["postMessage"](. req)

type listenerResp =
    | Cont
    | Stop
    | Unreg
    | StopAndUnreg

type listener = {
    id: int,
    callback: beResponse => listenerResp
}
let nextListenerId = ref(0)
let getNextListenerId = () => {
    nextListenerId.contents = nextListenerId.contents + 1
    nextListenerId.contents - 1
}
let listeners = []
let regWorkerListener = callback => {
    let id = getNextListenerId()
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
            | Cont => i.contents = i.contents + 1
            | Stop => i.contents = listeners->Js_array2.length
            | Unreg => {
                unregWorkerListener(listener.id)
            }
            | StopAndUnreg => {
                unregWorkerListener(listener.id)
                i.contents = listeners->Js_array2.length
            }
        }
    }
}
