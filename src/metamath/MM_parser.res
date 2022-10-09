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
    | Block({level:int, statements:array<mmAstNode>})
    | Var({symbols:array<string>})
    | Disj({vars:array<string>})
    | Floating({label:string, expr:array<string>})
    | Essential({label:string, expr:array<string>})
    | Axiom({label:string, expr:array<string>})
    | Provable({label:string, expr:array<string>, proof:proof})

type mmException = {
    msg:string,
    begin?:int,
}
exception MmException(mmException)

let isWhitespace = str => str == " " || str == "\t" || str == "\n" || str == "\r"

let textAt = (text,i) => {
    let textLength = text->Js_string2.length
    let lengthToShow = 20
    let ellipsis = if (i+lengthToShow < textLength) {"..."} else {""}
    "'" ++ text->Js.String2.substrAtMost(~from=i, ~length=lengthToShow) ++ ellipsis ++ "'"
}

let parseMmFile = (text:string): mmAstNode => {
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

    let textAt = textAt(text, _)

    let parseComment = (~beginIdx:int):mmAstNode => {
        switch readAllTextTill("$)") {
            | None => raise(MmException({msg:`A comment is not closed at ${textAt(beginIdx)}`}))
            | Some(commentText) => {begin:beginIdx, end:idx.contents-1, stmt:Comment({text:commentText})}
        }
    }

    let rec readNextToken = (~skipComments=true, ()):string => {
        if (!skipComments) {
            skipWhitespaces()
            let beginIdx = idx.contents
            while (!endOfFile.contents && !(ch.contents->isWhitespace)) {
                readNextChar()
            }
            text->Js_string2.substring(~from=beginIdx, ~to_=idx.contents)
        } else {
            let nextToken = ref(readNextToken(~skipComments=false, ()))
            while (nextToken.contents == "$(") {
                let _ = parseComment(~beginIdx=idx.contents)
                nextToken.contents = readNextToken(~skipComments=false, ())
            }
            nextToken.contents
        }
    }

    let readAllTokensTill = (tillToken:string):option<array<string>> => {
        let result = ref(None)
        let tokens = []
        while (result.contents->Belt_Option.isNone) {
            let token = readNextToken(())
            if (token == "") {
                result.contents = Some(None)
            } else if (token == "$(") {
                //skipping comments inside of statements
                let _ = parseComment(~beginIdx=idx.contents)
            } else if (token == tillToken) {
                result.contents = Some(Some(tokens))
            } else {
                let _ = tokens->Js_array2.push(token)
            }
        }
        result.contents->Belt_Option.getExn
    }

    let parseConst = (~beginIdx:int):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A constant statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Const({symbols:tokens})}
        }
    }

    let parseVar = (~beginIdx:int):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A variable statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Var({symbols:tokens})}
        }
    }

    let parseDisj = (~beginIdx:int):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A disjoint statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Disj({vars:tokens})}
        }
    }

    let parseFloating = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`A floating statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Floating({label, expr:tokens})}
        }
    }

    let parseEssential = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`An essential statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Essential({label, expr:tokens})}
        }
    }

    let parseAxiom = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$.") {
            | None => raise(MmException({msg:`An axiom statement is not closed at ${textAt(beginIdx)}`}))
            | Some(tokens) => {begin:beginIdx, end:idx.contents-1, stmt:Axiom({label, expr:tokens})}
        }
    }

    let parseProvable = (~beginIdx:int, ~label:string):mmAstNode => {
        switch readAllTokensTill("$=") {
            | None => raise(MmException({msg:`A provable statement is not closed[1] at ${textAt(beginIdx)}`}))
            | Some(expression) => {
                let firstProofToken = readNextToken(())
                if (firstProofToken == "(") {
                    switch readAllTokensTill(")") {
                        | None => raise(MmException({msg:`A provable statement is not closed[2] at ${textAt(beginIdx)}`}))
                        | Some(proofLabels) => {
                            switch readAllTokensTill("$.") {
                                | None => raise(MmException({msg:`A probale statement is not closed[3] at ${textAt(beginIdx)}`}))
                                | Some(compressedProofBlocks) =>
                                    {begin:beginIdx, end:idx.contents-1, stmt:Provable({label, expr:expression,
                                        proof:Compressed({labels:proofLabels, compressedProofBlock:""->Js_string2.concatMany(compressedProofBlocks)})
                                    })}
                            }
                        }
                    }
                } else {
                    switch readAllTokensTill("$.") {
                        | None => raise(MmException({msg:`A provable statement is not closed[4] at ${textAt(beginIdx)}`}))
                        | Some(proofLabels) =>
                            {
                                begin:beginIdx, end:idx.contents-1, 
                                stmt:Provable({label, expr:expression, proof:Uncompressed({labels:[firstProofToken]->Js.Array2.concat(proofLabels)})})
                            }
                    }
                }
            }
        }
    }

    let rec parseBlock = (~beginIdx:int, ~level:int):mmAstNode => {// parses text until $} token or until the end of text
        let result = ref(None)
        let statements = []

        let pushStmt = stmt => {
            let _ = statements->Js_array2.push(stmt)
        }

        while (result.contents->Belt_Option.isNone) {
            let token = readNextToken(~skipComments=false, ())
            let tokenIdx = idx.contents - token->Js_string2.length
            if (token == "") {
                if (level == 0) {
                    result.contents = Some({begin:beginIdx, end:idx.contents-1, stmt:Block({level, statements:statements})})
                } else {
                    raise(MmException({msg:`Unexpected end of a block. The block begins at ${textAt(beginIdx)} and is not closed.`}))
                }
            } else if (token == "$}") {
                result.contents = Some({begin:beginIdx, end:idx.contents-1, stmt:Block({level, statements:statements})})
            } else if (token == "${") {
                pushStmt(parseBlock(~beginIdx=tokenIdx, ~level=level+1))
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
                let token2 = readNextToken(())
                let token2Idx = idx.contents - token2->Js_string2.length
                if (token2 == "") {
                    raise(MmException({msg:`Unexpected end of file at ${textAt(tokenIdx)}`}))
                } else if (token2 == "$f") {
                    pushStmt(parseFloating(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$e") {
                    pushStmt(parseEssential(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$a") {
                    pushStmt(parseAxiom(~beginIdx=tokenIdx, ~label))
                } else if (token2 == "$p") {
                    pushStmt(parseProvable(~beginIdx=tokenIdx, ~label))
                } else {
                    raise(MmException({msg:`Unexpected token '${token2}' at ${textAt(token2Idx)}`}))
                }
            }
        }
        result.contents->Belt_Option.getExn
    }

    parseBlock(~beginIdx=idx.contents, ~level=0)
}

let traverseAllNodes = (context:'c, root:mmAstNode, consumer:('c,mmAstNode)=>option<'res>): option<'res> => {
    let nodesToProcess = Belt_MutableStack.make()
    nodesToProcess->Belt_MutableStack.push(root)
    let res = ref(None)
    while (!(nodesToProcess->Belt_MutableStack.isEmpty) && res.contents->Belt_Option.isNone) {
        switch nodesToProcess->Belt_MutableStack.pop {
            | Some(currNode) => {
                res.contents = consumer(context, currNode)
                if (res.contents->Belt_Option.isNone) {
                    switch currNode {
                        | {stmt:Block({statements})} => {
                            for i in statements->Js_array2.length - 1 downto 0 {
                                nodesToProcess->Belt_MutableStack.push(statements[i])
                            }
                        }
                        | _ => ()
                    }
                }
            }
            | None => ()
        }
    }
    res.contents
}

let stmtToStr: mmAstNode => array<string> = stmt => {
    open Expln_utils_common
    let level = ref(0)
    let res = []
    let _ = traverseAllNodes((), stmt, ((), node) => {
        let str = switch node {
            | {stmt:Comment({text})} => "$( " ++ text ++ " $)"
            | {stmt:Const({symbols})} =>  "$c " ++ symbols->strJoin(~sep=" ", ()) ++ " $."
            | {stmt:Block({level: newLevel})} => {
                level.contents = newLevel
                `begin block level=${i2s(newLevel)}`
            }
            | {stmt:Var({symbols})} =>  "$v " ++ symbols->strJoin(~sep=" ", ()) ++ " $."
            | {stmt:Disj({vars})} =>  "$d " ++ vars->strJoin(~sep=" ", ()) ++ " $."
            | {stmt:Floating({label, expr})} =>  label ++ " $f " ++ expr->strJoin(~sep=" ", ()) ++ " $."
            | {stmt:Essential({label, expr})} =>  label ++ " $e " ++ expr->strJoin(~sep=" ", ()) ++ " $."
            | {stmt:Axiom({label, expr})} =>  label ++ " $a " ++ expr->strJoin(~sep=" ", ()) ++ " $."
            | {stmt:Provable({label, expr, proof})} =>  label ++ " $p " ++ expr->strJoin(~sep=" ", ()) ++ " $= " ++ switch proof {
                | Uncompressed({labels}) => labels->strJoin(~sep=" ", ())
                | _ => "..."
            } ++ " $."
        }
        let _ = res->Js_array2.push(str->Js_string2.replaceByRe(%re("/[\n\r]/g"), " "))
        None
    })
    res
}