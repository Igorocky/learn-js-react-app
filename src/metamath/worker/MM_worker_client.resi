open MM_worker_api

let sendToBe: beRequest => unit
let registerBeListener: (beResponse => bool) => int
let unregisterBeListener: int => unit
