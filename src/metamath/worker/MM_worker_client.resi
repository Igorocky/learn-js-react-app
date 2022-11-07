open MM_worker_api

let sendToWorker: beRequest => unit
let regWorkerListener: (beResponse => bool) => int
let unregWorkerListener: int => unit
