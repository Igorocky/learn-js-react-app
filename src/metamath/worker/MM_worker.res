open MM_worker_api
open MM_parser

@val external sendToFe: beResponse => unit = "postMessage"

let parse = (senderId,text) => {
    try {
        let parseResult = parseMmFile(
            text,
            ~onProgress = pct => {
                sendToFe(MmFileParseProgress({senderId, pct}))
            },
            ()
        )
        sendToFe(MmFileParseProgress({senderId, pct:1.}))
        sendToFe(MmFileParsed({senderId, parseResult:Ok(parseResult)}))
    } catch {
        | MmException({msg}) => {
            sendToFe(MmFileParsed({senderId, parseResult:Error(msg)}))
        }
    }
}

let processRequest: beRequest => unit = req => {
    switch req {
        | ParseMmFile({senderId, mmFileText}) => parse(senderId, mmFileText)
    }
}
