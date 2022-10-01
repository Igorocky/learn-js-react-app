open MM_types
open MM_parserInput

let collectWhile = (inp:parserInput, predicate:(string,int) => result<bool,string>): parseResult<string> => {
    let length = Js_string2.length(inp.text)
    let i = ref(inp.begin)
    let continue = ref(true)
    let err = ref(None)
    while (continue.contents && i.contents<length) {
        switch predicate(inp.text, i.contents) {
            | Ok(b) => {
                continue.contents = b
            }
            | Error(msg) => {
                err.contents = Some(Error(msg))
                continue.contents = false
            }
        }
        i.contents = i.contents + 1
    }
    switch err.contents {
        | Some(err) => err
        | None => {
            let end = i.contents-2
            if (end < inp.begin) {
                Ok({result:"", end})
            } else {
                Ok({result: inp.text->Js.String2.substring(~from=inp.begin, ~to_=end+1), end})
            }
        }
    }
}

let parseComment = (inp: parserInput): parseResult<comment> => {
    if ("$" != inp->charAtRel(0) || "(" != inp->charAtRel(1)) {
        inp->err("cannot parse comment: the input doesn't begin with $(")
    } else {
        let text = inp.text
        let maxIdxForCommentEnd = text->Js_string2.length - 2
        let isEndOfComment = i => i <= maxIdxForCommentEnd && "$" == text->Js_string2.charAt(i) && ")" == text->Js_string2.charAt(i+1)
        let commentText = collectWhile(inp->proceed(2), (_,i) => {
            if (i > maxIdxForCommentEnd) {
                inp->err("cannot parse comment: comment is not closed [1]")
            } else {
                Ok(!isEndOfComment(i))
            }
        })
        switch commentText {
            | Ok(commentText) => {
                if (!isEndOfComment(commentText.end+1)) {
                    inp->err("cannot parse comment: comment is not closed [2]")
                } else {
                    let end = commentText.end+2
                    Ok({result: {text: commentText.result, beginIdx:inp.begin, endIdx: end}, end})
                }
            }
            | Error(msg) => Error(msg)
        }
    }
}

let isWhiteSpace = str => str == " " || str == "\t" || str == "\n" || str == "\r"

let extractComments = (text:string):parseResult<(array<comment>, array<nonComment>)> => {
    let comments = []
    let nonComments = []
    let idx = ref(0)
    let textLength = text->Js_string2.length
    let charAt = i => text->Js_string2.charAt(i)
    let charAtIdx = () => charAt(idx.contents)
    let charAtIdxPlus1 = () => charAt(idx.contents+1)
    let err = ref(None)
    while (err.contents->Belt_Option.isNone && idx.contents < textLength) {
        while (idx.contents < textLength && charAtIdx()->isWhiteSpace) {
            idx.contents = idx.contents + 1
        }
        if (idx.contents+1 < textLength && charAtIdx() == "$" && charAtIdxPlus1() == "(") {
            switch parseComment(makeParserInput2(text,idx.contents)) {
                | Ok(comment) => {
                    let _ = comments->Js_array2.push(comment.result)
                    idx.contents = comment.end+1
                }
                | Error(msg) => {
                    err.contents = Some(msg)
                }
            }
        } else {
            let beginOfNonCommentIdx = idx.contents
            while (idx.contents < textLength && !(idx.contents+1 < textLength && charAtIdx() == "$" && charAtIdxPlus1() == "(")) {
                idx.contents = idx.contents + 1
            }
            if (beginOfNonCommentIdx < idx.contents) {
                let _ = nonComments->Js_array2.push({
                    text: text->Js_string2.substring(~from=beginOfNonCommentIdx, ~to_=idx.contents),
                    beginIdx: beginOfNonCommentIdx,
                    endIdx: idx.contents-1,
                    lastComment: comments->Belt_Array.get(comments->Belt_Array.size-1)
                })
            }
        }
    }
    switch err.contents {
        | None => Ok({result:(comments, nonComments), end: textLength-1})
        | Some(msg) => Error(msg)
    }
}