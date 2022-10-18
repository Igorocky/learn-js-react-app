open MM_parser

type state = Balanced | Opened | Failed

type rec paren = {
    code: int,
    isOpen: bool,
    opposite: int,
}

type parenCnt = {
    parens: array<paren>,
    parentStack: array<paren>,
    mutable failed: bool
}

let parenCntMake: (~begin:array<int>, ~end:array<int>) => parenCnt = (~begin:array<int>, ~end:array<int>) => {
    if (begin->Js_array2.length != end->Js_array2.length) {
        raise(MmException({msg:`begin->Js_array2.length != end->Js_array2.length`}))
    } else {
        {
            parens:
                begin->Js_array2.mapi((code,i) => {code, isOpen:true, opposite:end[i]})
                    ->Js_array2.concat(
                        end->Js_array2.mapi((code,i) => {code, isOpen:false, opposite:begin[i]})
                    ),
            parentStack: [],
            failed: false,
        }
    }
}

let parenCntReset: parenCnt => unit = cnt => {
    cnt.parentStack->Expln_utils_common.clearArray
    cnt.failed = false
}

let parenCntPut: (parenCnt,int) => state = (cnt,i) => {
    if (!cnt.failed) {
        switch cnt.parens->Js_array2.find(({code}) => code == i) {
            | Some(paren) => {
                if (paren.isOpen) {
                    cnt.parentStack->Js_array2.push(paren)->ignore
                } else {
                    switch cnt.parentStack->Js_array2.pop {
                        | None => cnt.failed = true
                        | Some(lastParen) => {
                            if (lastParen.opposite != paren.code) {
                                cnt.failed = true
                            }
                        }
                    }
                }
            }
        }
    }
    if (cnt.failed) {
        Failed
    } else if (cnt.parentStack->Js_array2.length == 0) {
        Balanced
    } else {
        Opened
    }
}