open MM_context
open MM_substitution
open MM_parser

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    proved:bool,
    dist:int,
    src: option<array<exprSource>>
}

type proofTable = array<proofRecord>

let createSyntaxProofTable: expr => array<proofRecord> = exprToProve => {
    [{
        expr:exprToProve,
        proved:false,
        dist:0,
        src: None
    }]
}

let getExprToProve: proofTable => option<int> = tbl => {
    let minDist = ref(tbl->Js_array2.length)
    let res = ref(-1)
    let maxI = tbl->Js_array2.length-1
    for i in 0 to maxI {
        let r = tbl[i]
        if (!r.proved && r.dist < minDist.contents) {
            minDist.contents = r.dist
            res.contents = i
        }
    }
    if res.contents >= 0 { Some(res.contents) } else { None }
}