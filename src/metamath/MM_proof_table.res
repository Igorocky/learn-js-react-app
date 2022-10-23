open MM_context
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
                        | _ => ()
                    }
                })
            }
        }
    }
}

module ExprCmp = Belt.Id.MakeComparable({
    type t = expr
    let cmp = (e1,e2) => {
        let len1 = e1->Js_array2.length
        let len2 = e2->Js_array2.length
        switch Expln_utils_common.intCmp(len1, len2) {
            | 0 => {
                let res = ref(0)
                let i = ref(0)
                while (i.contents < len1 && res.contents == 0) {
                    res.contents = Expln_utils_common.intCmp(e1[i.contents], e2[i.contents])
                    i.contents = i.contents + 1
                }
                res.contents
            }
            | r => r
        }
    }
})

let getChildren = src => {
    switch r.src {
        | None => raise(MmException({msg: `All records in a proofTable must have non empty source.`}))
        | Some([Assertion({args})]) => Some(args->Js_array2.map(a=>tbl[a]))
        | Some([Hypothesis(_)]) => None
        | _ => raise(MmException({
            msg: `Unexpected source of a record in a proofTable: ${Expln_utils_common.stringify(r.src)}`
        }))
    }
}

let traverseRecords = (ctx,tbl,~onProcess,~onReprocess) => {
    let passedExprs = Belt_MutableSet.make(~id=module(ExprCmp))
    let repeatedExprsSet = Belt_MutableSet.make(~id=module(ExprCmp))
    let repeatedExprs = []
    Expln_utils_data.traverseTree(
        (),
        tbl[0],
        (_, r) => if (passedExprs->Belt_MutableSet.has(r.expr)) { None } else { getChildren(r.src) } ,
        ~process = (_, r) => {
            if (passedExprs->Belt_MutableSet.has(r.expr)) {
                let reprocessFirstTime = !(repeatedExprsSet->Belt_MutableSet.has(r.expr))
                if (reprocessFirstTime) {
                    repeatedExprsSet->Belt_MutableSet.add(r.expr)
                }
                onReprocess(r,reprocessFirstTime)
            } else {
                passedExprs->Belt_MutableSet.add(r.expr)
                onProcess(r)
            }
            None
        },
        ()
    )->ignore
}

let findReusedExprs = (ctx,tbl):array<expr> => {
    let reusedExprs = []
    traverseRecords(ctx,tbl,
        ~onProcess = _ => (),
        ~onReprocess = (r,first) => {
            if (first) {
                reusedExprs->Js_array2.push(r.expr)->ignore
            }
        }
    )
    reusedExprs
}

let createProof = (ctx:mmContext, label:string, tbl:proofTable):proof => {
    let tblLen = tbl->Js_array2.length
    if (tblLen == 0) {
        raise(MmException({msg:`Cannot extract a proof from empty proofTable.`}))
    }
    let frame = createFrame(ctx, label, ctx->ctxExprToStr(tbl[0].expr))
    let mandHypLabelToInt = Belt_MapString.fromArray(
        frame.hyps->Js_array2.mapi(({label}, i) => (label, i+1))
    )
    let manHypLen = mandHypLabelToInt->Belt_MapString.size
    let labels = []
    let reusedExprs = findReusedExprs(ctx,tbl)
    let reusedExprToInt = reusedExprs
        ->Js_array2.mapi((expr,i) => (expr,manHypLen+i+1))
        ->Belt_Map.fromArray(~id=module(ExprCmp))
    let proofSteps = []
    let passedExprs = Belt_MutableSet.make(~id=module(ExprCmp))
    traverseRecords(ctx,tbl,
        ~onProcess = r => {
            switch r.src {
                | Some([Hypothesis({label})]) => None
                | Some([Assertion({args})]) => Some(args->Js_array2.map(a=>tbl[a]))
                | _ => ()
            }
        },
        ~onReprocess = (r,first) => {
            if (first) {
                reusedExprs->Js_array2.push(r.expr)->ignore
            }
        }
    )
    Expln_utils_data.traverseTree(
        (),
        tbl[0],
        (_, r) => if (passedExprs->Belt_MutableSet.has(r.expr)) { None } else { getChildren(r.src) } ,
        ~process = (_, r) => {
            if (passedExprs->Belt_MutableSet.has(r.expr)) {
                proofSteps->Js_array2.push(reusedExprToInt->Belt_Map.getExn(r.expr))
            } else {
                passedExprs->Belt_MutableSet.add(r.expr)
            }
            None
        },
        ()
    )->ignore

}