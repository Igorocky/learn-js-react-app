open MM_worker_api

@val external webworker: {..} = "window.webWorkerInst"
let sendToWorker: beRequest => unit = req => webworker["postMessage"](. req)


type listener = {
    id: int,
    callback: beResponse => bool
}
let nextListenerId = ref(0)
let getNextListenerId = () => {
    nextListenerId.contents = nextListenerId.contents + 1
    nextListenerId.contents - 1
}
let listeners = ref([])
let regWorkerListener = callback => {
    let id = getNextListenerId()
    listeners.contents->Js_array2.push({ id, callback })->ignore
    id
}
let unregWorkerListener = id => {
    let i = ref(0)
    while (i.contents < listeners.contents->Js_array2.length) {
        if (listeners.contents[i.contents].id == id) {
            listeners.contents->Js_array2.removeCountInPlace(~pos=i.contents, ~count=1)->ignore
        } else {
            i.contents = i.contents + 1
        }
    }
}
webworker["onmessage"] = msg => {
    let resp = msg["data"]
    listeners.contents->Expln_utils_common.arrForEach(l => if l.callback(resp) {Some(())} else {None})->ignore
}