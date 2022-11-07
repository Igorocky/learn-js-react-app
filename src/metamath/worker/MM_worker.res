open MM_fe_be
open MM_parser

@val external sendToFe: beResponse => unit = "postMessage"

let parse = (senderId,text) => {
    try {
        let ast = parseMmFile(
            text,
            ~onProgress = pct => {
                sendToFe(MmFileParseProgress({senderId, pct}))
            },
            ()
        )
        sendToFe(MmFileParsed({senderId, ast:Ok(ast)}))
    } catch {
        | MmException({msg}) => {
            sendToFe(MmFileParsed({senderId, ast:Error(msg)}))
        }
    }
}

let processRequest: beRequest => unit = req => {
    switch req {
        | ParseMmFile({senderId, mmFileText}) => parse(senderId, mmFileText)
    }
}
