type proof =
    | Uncompressed({labels:array<string>})
    | Compressed({labels:array<string>, compressedProofBlock:string})

type rec mmAstNode = {
    begin: int,
    end: int,
    stmt: stmt
}
and stmt =
    | Comment({text:string})
    | Const({symbols:array<string>})
    | Block({statements:array<mmAstNode>})
    | Var({symbols:array<string>})
    | Disj({vars:array<string>})
    | Floating({label:string, expr:array<string>})
    | Essential({label:string, expr:array<string>})
    | Axiom({label:string, expr:array<string>})
    | Provable({label:string, expr:array<string>, proof:proof})


let isWhitespace = str => str == " " || str == "\t" || str == "\n" || str == "\r"

let textAt = (text,i) => {
    let textLength = text->Js_string2.length
    let lengthToShow = 20
    let ellipsis = if (i+lengthToShow < textLength) {"..."} else {""}
    "'" ++ text->Js.String2.substrAtMost(~from=i, ~length=lengthToShow) ++ ellipsis ++ "'"
}

let parseMmFile = (text:string): result<mmAstNode,string> => {
    let textLength = text->Js_string2.length
    let idx = ref(0) // index of the next char to read.
    let endOfFile = ref(false) // if idx is outside of text then endOfFile is true.
    let ch = ref("") // the char idx is pointing to. If endOfFile then ch == "".

    let setIdx = i => {
        idx.contents = i
        if (idx.contents >= textLength) {
            endOfFile.contents = true
            ch.contents = ""
        } else {
            endOfFile.contents = false
            ch.contents = text->Js_string2.charAt(idx.contents)
        }
    }
    setIdx(0)

    let readNextChar = ():unit => {
        if (!endOfFile.contents) {
            idx.contents = idx.contents+1
            if (idx.contents >= textLength) {
                endOfFile.contents = true
                ch.contents = ""
            } else {
                ch.contents = text->Js_string2.charAt(idx.contents)
            }
        }
    }

    let skipWhitespaces = ():unit => {// makes sure the next char to process is not a whitespace or the text is read to the end.
        while (!endOfFile.contents && ch.contents->isWhitespace) {
            readNextChar()
        }
    }

    let readNextToken = ():string => {
        skipWhitespaces()
        let beginIdx = idx.contents
        while (!endOfFile.contents && !(ch.contents->isWhitespace)) {
            readNextChar()
        }
        text->Js_string2.substring(~from=beginIdx, ~to_=idx.contents)
    }

    let readAllTextTill = (tillToken:string):option<string> => {
        let result = ref(None)
        let beginIdx = idx.contents
        while (result.contents->Belt_Option.isNone) {
            let foundIdx = text->Js_string2.indexOfFrom(tillToken, idx.contents)
            if (foundIdx < 0) {
                result.contents = Some(None)
            } else {
                let nextIdx = foundIdx + tillToken->Js_string2.length
                setIdx(nextIdx)
                if (endOfFile.contents || ch.contents->isWhitespace) {
                    result.contents = Some(Some(text->Js_string2.substring(~from=beginIdx, ~to_=foundIdx)))
                } else {
                    setIdx(foundIdx+1)
                }
            }
        }
        result.contents->Belt_Option.getExn
    }

    let readAllTokensTill = (tillToken:string):option<array<string>> => {
        let result = ref(None)
        let tokens = []
        while (result.contents->Belt_Option.isNone) {
            let token = readNextToken()
            if (token == "") {
                result.contents = Some(None)
            } else if (token == tillToken) {
                result.contents = Some(Some(tokens))
            } else {
                let _ = tokens->Js_array2.push(token)
            }
        }
        result.contents->Belt_Option.getExn
    }

    let textAt = textAt(text, _)

    let parseComment = (~beginIdx:int):result<mmAstNode,string> => {
        switch readAllTextTill("$)") {
            | None => Error(`A comment is not closed at ${textAt(beginIdx)}`)
            | Some(commentText) => Ok({begin:beginIdx, end:idx.contents-1, stmt:Comment({text:commentText})})
        }
    }
    
    let parseConst = (~beginIdx:int):result<mmAstNode,string> => {
        switch readAllTokensTill("$.") {
            | None => Error(`A constant statement is not closed at ${textAt(beginIdx)}`)
            | Some(tokens) => Ok({begin:beginIdx, end:idx.contents-1, stmt:Const({symbols:tokens})})
        }
    }

    let parseVar = (~beginIdx:int):result<mmAstNode,string> => {
        switch readAllTokensTill("$.") {
            | None => Error(`A variable statement is not closed at ${textAt(beginIdx)}`)
            | Some(tokens) => Ok({begin:beginIdx, end:idx.contents-1, stmt:Var({symbols:tokens})})
        }
    }

    let parseDisj = (~beginIdx:int):result<mmAstNode,string> => {
        switch readAllTokensTill("$.") {
            | None => Error(`A disjoint statement is not closed at ${textAt(beginIdx)}`)
            | Some(tokens) => Ok({begin:beginIdx, end:idx.contents-1, stmt:Disj({vars:tokens})})
        }
    }

    let parseFloating = (~beginIdx:int, ~label:string):result<mmAstNode,string> => {
        switch readAllTokensTill("$.") {
            | None => Error(`A floating statement is not closed at ${textAt(beginIdx)}`)
            | Some(tokens) => Ok({begin:beginIdx, end:idx.contents-1, stmt:Floating({label, expr:tokens})})
        }
    }

    let parseEssential = (~beginIdx:int, ~label:string):result<mmAstNode,string> => {
        switch readAllTokensTill("$.") {
            | None => Error(`An essential statement is not closed at ${textAt(beginIdx)}`)
            | Some(tokens) => Ok({begin:beginIdx, end:idx.contents-1, stmt:Essential({label, expr:tokens})})
        }
    }

    let parseAxiom = (~beginIdx:int, ~label:string):result<mmAstNode,string> => {
        switch readAllTokensTill("$.") {
            | None => Error(`An axiom statement is not closed at ${textAt(beginIdx)}`)
            | Some(tokens) => Ok({begin:beginIdx, end:idx.contents-1, stmt:Axiom({label, expr:tokens})})
        }
    }

    let parseProvable = (~beginIdx:int, ~label:string):result<mmAstNode,string> => {
        switch readAllTokensTill("$=") {
            | None => Error(`A probale statement is not closed[1] at ${textAt(beginIdx)}`)
            | Some(expression) => {
                let firstProofToken = readNextToken()
                if (firstProofToken == "(") {
                    switch readAllTokensTill(")") {
                        | None => Error(`A probale statement is not closed[2] at ${textAt(beginIdx)}`)
                        | Some(proofLabels) => {
                            switch readAllTokensTill("$.") {
                                | None => Error(`A probale statement is not closed[3] at ${textAt(beginIdx)}`)
                                | Some(compressedProofBlocks) => 
                                    Ok({begin:beginIdx, end:idx.contents-1, stmt:Provable({label, expr:expression,
                                        proof:Compressed({labels:proofLabels, compressedProofBlock:""->Js_string2.concatMany(compressedProofBlocks)})
                                    })})
                            }
                        }
                    }
                } else {
                    switch readAllTokensTill("$.") {
                        | None => Error(`A probale statement is not closed[4] at ${textAt(beginIdx)}`)
                        | Some(proofLabels) => 
                            Ok({begin:beginIdx, end:idx.contents-1, stmt:Provable({label, expr:expression, proof:Uncompressed({labels:proofLabels})})})
                    }
                }
            }
        }
    }

    let rec parseBlock = (~beginIdx:int, ~level:int):result<mmAstNode,string> => {// parses text until $} token or until the end of text
        let result = ref(None)
        let statements = []

        let pushStmt = stmt =>
            switch stmt {
                | Error(msg) => result.contents = Some(Error(msg))
                | Ok(stmt) => let _ = statements->Js_array2.push(stmt)
            }

        while (result.contents->Belt_Option.isNone) {
            let token = readNextToken()
            let tokenIdx = idx.contents - token->Js_string2.length
            if (token == "") {
                if (level == 0) {
                    result.contents = Some(Ok({begin:beginIdx, end:idx.contents-1, stmt:Block({statements:statements})}))
                } else {
                    result.contents = Some(Error(`Unexpected end of a block. The block begins at ${textAt(beginIdx)} and is not closed.`))
                }
            } else if (token == "$}") {
                result.contents = Some(Ok({begin:beginIdx, end:idx.contents-1, stmt:Block({statements:statements})}))
            } else if (token == "${") {
                switch parseBlock(~beginIdx=tokenIdx, ~level=level+1) {
                    | Error(msg) => result.contents = Some(Error(msg))
                    | Ok(stmt) => let _ = statements->Js_array2.push(stmt)
                }
            } else if (token == "$(") {
                pushStmt(parseComment(~beginIdx=tokenIdx))
            } else if (token == "$c") {
                pushStmt(parseConst(~beginIdx=tokenIdx))
            } else if (token == "$v") {
                pushStmt(parseVar(~beginIdx=tokenIdx))
            } else if (token == "$d") {
                pushStmt(parseDisj(~beginIdx=tokenIdx))
            } else {
                let label = token
                let token2 = readNextToken()
                let token2Idx = idx.contents - token2->Js_string2.length
                if (token2 == "") {
                    result.contents = Some(Error(`Unexpected end of file at ${textAt(tokenIdx)}`))
                } else if (token2 == "$f") {
                    pushStmt(parseFloating(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$e") {
                    pushStmt(parseEssential(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$a") {
                    pushStmt(parseAxiom(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$p") {
                    pushStmt(parseProvable(~beginIdx=tokenIdx, ~label))
                } else {
                    result.contents = Some(Error(`Unexpected token '${token2}' at ${textAt(token2Idx)}`))
                }
            }
        }
        result.contents->Belt_Option.getExn
    }

    parseBlock(~beginIdx=idx.contents, ~level=0)
}