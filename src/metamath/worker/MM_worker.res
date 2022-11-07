open MM_worker_api
open MM_parser

@val external sendToUi: beResponse => unit = "postMessage"

let parse = (senderId,text) => {
    try {
        let parseResult = parseMmFile(
            text,
            ~onProgress = pct => {
                sendToUi(MmFileParseProgress({senderId, pct}))
            },
            ()
        )
        sendToUi(MmFileParseProgress({senderId, pct:1.}))
        sendToUi(MmFileParsed({senderId, parseResult:Ok(parseResult)}))
    } catch {
        | MmException({msg}) => {
            sendToUi(MmFileParsed({senderId, parseResult:Error(msg)}))
        }
    }
}

let processRequest: beRequest => unit = req => {
    switch req {
        | ParseMmFile({senderId, mmFileText}) => parse(senderId, mmFileText)
    }
}
