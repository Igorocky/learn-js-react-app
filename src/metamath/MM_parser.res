open MM_parserInput

type proof =
    | Uncompressed(array<string>)
    | Compressed(array<string>, string)

type rec stmt =
    | Comment(int, int, string)
    | Const(int, int, array<string>)
    | Block(int, int, array<stmt>)
    | Var(int, int, array<string>)
    | Disj(int, int, array<string>)
    | Floating(int, int, string, string, string)
    | Essential(int, int, string, array<string>)
    | Axiom(int, int, string, array<string>)
    | Provable(int, int, string, array<string>, proof)

type mmAstNode = {
    beginIdx: int,
    endIdx: int,
    stmt: stmt,
}

let isWhitespace = str => str == " " || str == "\t" || str == "\n" || str == "\r"

let parseMmFile = (text:string): result<stmt,string> => {
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

    let readAllTill = (tillToken:string):option<string> => {
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

    let textAt = i => makeParserInput2(text, i)->currPositionStr

    let parseComment = (~beginIdx:int):result<stmt,string> => {
        switch readAllTill("$)") {
            | None => Error(`A comment is not closed at ${textAt(beginIdx)}`)
            | Some(commentText) => Ok(Comment(beginIdx, idx.contents-1, commentText))
        }
    }

    let parseBlock = (~beginIdx:int, ~level:int):result<stmt,string> => {// parses text until $} token or until the end of text
        let result = ref(None)
        let statements = []
        while (result.contents->Belt_Option.isNone) {
            let token = readNextToken()
            let tokenIdx = idx.contents - token->Js_string2.length
            if (token == "") {
                if (level == 0) {
                    result.contents = Some(Ok(Block(beginIdx, idx.contents-1, statements)))
                } else {
                    result.contents = Some(Error(`Unexpected end of a block. The block begins at ${textAt(beginIdx)} and is not closed.`))
                }
            } else if (token == "$}") {
                result.contents = Some(Ok(Block(beginIdx, idx.contents-1, statements)))
            } else if (token == "$(") {
                switch parseComment(~beginIdx=tokenIdx) {
                    | Error(msg) => result.contents = Some(Error(msg))
                    | Ok(comment) => let _ = statements->Js_array2.push(comment)
                }
            } else {
                result.contents = Some(Error(`Unexpected token '${token}' at ${textAt(tokenIdx)}`))
            }
        }
        result.contents->Belt_Option.getExn
    }

    parseBlock(~beginIdx=idx.contents, ~level=0)
}