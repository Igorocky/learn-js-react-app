open MM_worker_api
open MM_parser
open MM_context

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

let loadMmContext = (senderId, scopes) => {
    let totalNumOfAssertions = scopes->Js_array2.reduce((a,e) => a+e.expectedNumOfAssertions, 0)->Belt_Int.toFloat
    let weights = scopes->Js_array2.map(s => s.expectedNumOfAssertions->Belt_Int.toFloat /. totalNumOfAssertions)
    try {
        let i = ref(0)
        let len = scopes->Js_array2.length
        let ctx = ref(createContext(()))
        while (i.contents < len) {
            let scope = scopes[i.contents]
            let basePct = weights->Js_array2.reducei((a,w,idx) => if idx < i.contents {a +. w} else {a}, 0.)
            ctx.contents = loadContext(
                scopes[i.contents].ast,
                ~initialContext=ctx.contents,
                ~stopBefore=?scope.stopBefore,
                ~stopAfter=?scope.stopAfter,
                ~expectedNumOfAssertions=scope.expectedNumOfAssertions,
                ~onProgress = pct => {
                    sendToUi(MmContextLoadProgress({senderId, pct: basePct +. pct}))
                },
                ()
            )
            i.contents = i.contents + 1
        }
        sendToUi(MmContextLoadProgress({senderId, pct: 1.}))
        sendToUi(MmContextLoaded({senderId, ctx:Ok(ctx.contents)}))
    } catch {
        | MmException({msg}) => {
            sendToUi(MmContextLoaded({senderId, ctx:Error(msg)}))
        }
    }
}

let processRequest: beRequest => unit = req => {
    switch req {
        | ParseMmFile({senderId, mmFileText}) => parse(senderId, mmFileText)
        | LoadMmContext({senderId, scopes}) => loadMmContext(senderId, scopes)
    }
}
