let procName: string
type request
type response
let calcTextLength: (~text: string, ~onDone: int => unit) => unit
let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
