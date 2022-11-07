open MM_worker_api

type listenerResp =
    | Cont
    | Stop
    | Unreg
    | StopAndUnreg

let sendToWorker: beRequest => unit
let regWorkerListener: (beResponse => listenerResp) => int
let unregWorkerListener: int => unit
