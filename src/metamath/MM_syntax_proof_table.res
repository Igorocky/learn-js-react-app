open MM_context
open MM_substitution
open MM_parser

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    mutable proved:bool,
    mutable dist:int,
    mutable src: option<array<exprSource>>
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

let getNextExprToProveIdx: proofTable => option<int> = tbl => {
    let minDist = ref(tbl->Js_array2.length)
    let res = ref(-1)
    let maxI = tbl->Js_array2.length-1
    for i in 0 to maxI {
        let r = tbl[i]
        if (!r.proved && r.src->Belt.Option.isNone && r.dist < minDist.contents) {
            minDist.contents = r.dist
            res.contents = i
        }
    }
    if res.contents >= 0 { Some(res.contents) } else { None }
}

let addExprToProve: (proofTable, expr) => int = (tbl,expr) => {
    let len = tbl->Js_array2.length
    let found = ref(None)
    let i = ref(0)
    while (found.contents->Belt_Option.isNone && i.contents < len) {
        if (expr->exprEq(tbl[i.contents].expr)) {
            found.contents = Some(i.contents)
        }
        i.contents = i.contents + 1
    }
    switch found.contents {
        | Some(i) => i
        | None => {
            tbl->Js_array2.push({
                expr,
                proved: false,
                dist: -1,
                src: None
            })->ignore
            len
        }
    }
}

let markProved: proofTable => unit = tbl => {
    let provedIdxs = Belt_MutableSetInt.make()
    tbl->Js_array2.forEachi(({proved},i) => if proved {provedIdxs->Belt_MutableSetInt.add(i)})

    let thereIsNewProvedExpr = ref(true)
    while (thereIsNewProvedExpr.contents) {
        thereIsNewProvedExpr.contents = false
        tbl->Js_array2.forEachi((proofRec,proofRecIdx) => {
            if (!proofRec.proved && proofRec.src->Belt_Option.isSome) {
                let validProof = switch proofRec.src {
                    | None => None
                    | Some(proofs) => {
                        proofs->Expln_utils_common.arrForEach(proof => {
                            switch proof {
                                | Hypothesis(_) => Some(proof)
                                | Assertion({args}) if args->Js_array2.every(provedIdxs->Belt_MutableSetInt.has) => Some(proof)
                                | _ => None
                            }
                        })
                    }
                }
                switch validProof {
                    | None => ()
                    | Some(proof) => {
                        proofRec.src = Some([proof])
                        proofRec.proved = true
                        thereIsNewProvedExpr.contents = true
                        provedIdxs->Belt_MutableSetInt.add(proofRecIdx)
                    }
                }
            }
        })
    }
}

let updateDist: proofTable => unit = tbl => {
    let queue = Belt_MutableQueue.make()
    queue->Belt_MutableQueue.add(tbl[0])
    while (!(queue->Belt_MutableQueue.isEmpty)) {
        let parent = queue->Belt_MutableQueue.pop->Belt_Option.getExn
        let childDist = parent.dist
        switch parent.src {
            | None => ()
            | Some(proofs) => {
                proofs->Js_array2.forEach(proof => {
                    switch proof {
                        | Assertion({args}) => {
                            args->Js_array2.forEach(argIdx => {
                                let child = tbl[argIdx]
                                child.dist = childDist
                                queue->Belt_MutableQueue.add(child)
                            })
                        }
                    }
                })
            }
        }
    }
}