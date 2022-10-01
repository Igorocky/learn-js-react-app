open MM_types
open MM_parserInput

let extractComments = (text:string):(array<comment>, array<nonComment>) => {
    let comments = []
    let nonComments = []
    let idx = ref(0)



    (comments, nonComments)
}

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