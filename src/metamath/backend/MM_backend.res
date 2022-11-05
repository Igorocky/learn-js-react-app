open MM_fe_be

@val external sendToFe: beResponse => unit = "postMessage"

let processRequest: beRequest => unit = req => {
    switch req {
        | LogMsg(msg) => {
            Js.Console.log2("webworker.LogMsg: ", msg)
            sendToFe(LogDone)
        }
    }
}