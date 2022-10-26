open MM_context
open MM_parser
open MM_proof_verifier

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

let createProofTable: expr => array<proofRecord> = exprToProve => {
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

let getChildren = (tbl,src) => {
    switch src {
        | None => raise(MmException({msg: `All records in a proofTable must have non empty source.`}))
        | Some([Assertion({args})]) => Some(args->Js_array2.map(a=>tbl[a]))
        | Some([Hypothesis(_)]) => None
        | _ => raise(MmException({
            msg: `Unexpected source of a record in a proofTable: ${Expln_utils_common.stringify(src)}`
        }))
    }
}

let traverseRecordsInRpnOrder = (ctx,tbl,~onProcess,~onReprocess) => {
    let passedExprs = Belt_MutableSet.make(~id=module(ExprCmp))
    let repeatedExprsSet = Belt_MutableSet.make(~id=module(ExprCmp))
    let repeatedExprs = []
    Expln_utils_data.traverseTree(
        (),
        tbl[0],
        (_, r) => if (passedExprs->Belt_MutableSet.has(r.expr)) { None } else { getChildren(tbl,r.src) } ,
        ~process = (_, r) => {
            switch r.src {
                | Some([Hypothesis(_)]) => onProcess(r)
                | _ => ()
            }
            None
        },
        ~postProcess = (_, r) => {
            switch r.src {
                | Some([Assertion(_)]) => {
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
                }
                | _ => ()
            }
            None
        },
        ()
    )->ignore
}

let findReusedExprs = (ctx,tbl):array<expr> => {
    let reusedExprs = []
    traverseRecordsInRpnOrder(ctx,tbl,
        ~onProcess = _ => (),
        ~onReprocess = (r,first) => {
            if (first) {
                reusedExprs->Js_array2.push(r.expr)->ignore
            }
        }
    )
    reusedExprs
}

let createProof = (ctx:mmContext, tbl:proofTable):proof => {
    let tblLen = tbl->Js_array2.length
    if (tblLen == 0) {
        raise(MmException({msg:`Cannot extract a proof from empty proofTable.`}))
    }
    if (!tbl[0].proved) {
        raise(MmException({msg:`The first record in a proofTable must be proved.`}))
    }
    let mandHyps = getMandHyps(ctx, tbl[0].expr)
    let mandHypLabelToInt = Belt_MapString.fromArray(
        mandHyps->Js_array2.mapi(({label}, i) => (label, i+1))
    )
    let mandHypLen = mandHypLabelToInt->Belt_MapString.size
    let labels = []
    let labelToInt = label => {
        mandHypLen + switch labels->Js_array2.indexOf(label) {
            | -1 => labels->Js_array2.push(label)
            | i => i+1
        }
    }
    let reusedExprs = findReusedExprs(ctx,tbl)
    let reusedExprToInt = reusedExprs
        ->Js_array2.mapi((expr,i) => (expr,i+1))
        ->Belt_Map.fromArray(~id=module(ExprCmp))
    let proofSteps = []
    traverseRecordsInRpnOrder(ctx,tbl,
        ~onProcess = r => {
            let idx = switch r.src {
                | Some([Hypothesis({label})]) => {
                    switch mandHypLabelToInt->Belt_MapString.get(label) {
                        | Some(i) => i
                        | None => labelToInt(label)
                    }
                }
                | Some([Assertion({label})]) => labelToInt(label)
                | _ => raise(MmException({msg:`Cannot determine index of a proof step.`}))
            }
            proofSteps->Js_array2.push(idx)->ignore
            if (reusedExprToInt->Belt_Map.has(r.expr)) {
                proofSteps->Js_array2.push(0)->ignore
            }
        },
        ~onReprocess = (r,_) => {
            proofSteps->Js_array2.push(-(reusedExprToInt->Belt_Map.getExn(r.expr)))->ignore
        }
    )
    let labelsLastIdx = mandHypLen + labels->Js.Array2.length
    Compressed({
        labels,
        compressedProofBlock: proofSteps->Js_array2.map(i => {
            if (i == 0) {
                "Z"
            } else if (i < 0) {
                intToCompressedProofStr(labelsLastIdx - i)
            } else {
                intToCompressedProofStr(i)
            }
        })->Expln_utils_common.strJoin(~sep="", ())
    })

}

let printAsrtApplication = (ctx,args,label) => {
    args
        ->Js_array2.map(e=>ctxExprToStr(ctx,e)->Expln_utils_common.strJoin(~sep=" ", ()))
        -> Expln_utils_common.strJoin(~sep=", ", ())
        ++ " : " ++ label
}

let createProofTableFromProof: (mmContext, proofNode) => proofTable = (ctx,proofNode) => {
    let tbl = []
    Expln_utils_data.traverseTree(
        (),
        proofNode,
        (_, n) => {
            switch n {
                | Hypothesis(_) => None
                | Calculated({args}) => Some(args)
                //| Calculated({args, asrtLabel, expr}) => {
                    //switch tbl->Js.Array2.find(r => r.expr->exprEq(expr)) {
                        //| None => Some(args)
                        //| Some(existingRecord) => {
                            //if (!existingRecord.proved) {
                                //raise(MmException({msg:`!existingRecord.proved`}))
                            //}
                            //switch existingRecord.src {
                                //| Some([Assertion({args:existingArgs, label:existingLabel})]) => {
                                    //let existingArgsExpr = existingArgs->Js_array2.map(i=>tbl[i])->Js_array2.map(r=>r.expr)
                                    //let newArgsExpr = args->Js_array2.map(getExprFromNode)
                                    //if (existingArgsExpr == newArgsExpr && existingLabel == asrtLabel) {
                                        //None
                                    //} else {
                                        //raise(MmException({msg:`Unexpected condition in getChildNodes in Calculated: ${printAsrtApplication(ctx, existingArgs->Js_array2.map(i=>tbl[i])->Js_array2.map(r=>r.expr), existingLabel)} vs ${printAsrtApplication(ctx,args->Js_array2.map(getExprFromNode), asrtLabel)}`}))
                                    //}
                                //}
                                //| _ => raise(MmException({msg:`Unexpected condition in getChildNodes in Calculated.`}))
                            //}
                        //}
                    //}
                //}
            }
        },
        ~process = (_,n) => {
            switch n {
                | Hypothesis({hypLabel,expr}) => {
                    let i = tbl->addExprToProve(expr)
                    if (tbl[i].proved) {
                        switch tbl[i].src {
                            | Some([Hypothesis({label:existingHypLabel})]) => {
                                if (hypLabel != existingHypLabel) {
                                    raise(MmException({msg:`tbl[i].proved: ${existingHypLabel} vs Hypothesis({${hypLabel},${ctxExprToStr(ctx,expr)->Expln_utils_common.strJoin(~sep=" ", ())}})`}))
                                }
                            }
                            | _ => raise(MmException({msg:`tbl[i].proved in Hypothesis.`}))
                        }
                    } else {
                        tbl[i].proved = true
                        tbl[i].src = Some([Hypothesis({label:hypLabel})])
                    }
                }
                | Calculated({args, asrtLabel, expr}) => {
                    let i = tbl->addExprToProve(expr)
                    if (tbl[i].proved) {
                        //raise(MmException({msg:`tbl[i].proved in Calculated.`}))
                        switch tbl[i].src {
                            | Some([Assertion({args:existingArgs, label:existingLabel})]) => {
                                let existingArgsExpr = existingArgs->Js_array2.map(i=>tbl[i])->Js_array2.map(r=>r.expr)
                                let newArgsExpr = args->Js_array2.map(getExprFromNode)
                                if (existingArgsExpr != newArgsExpr || existingLabel != asrtLabel) {
                                    raise(MmException({msg:`tbl[i].proved in Calculated: ${printAsrtApplication(ctx,existingArgs->Js_array2.map(i=>tbl[i])->Js_array2.map(r=>r.expr), existingLabel)} vs ${printAsrtApplication(ctx, args->Js_array2.map(getExprFromNode), asrtLabel)}`}))
                                }
                            }
                            | _ => raise(MmException({msg:`tbl[i].proved in Calculated.`}))
                        }
                    } else {
                        tbl[i].proved = true
                        tbl[i].src = Some([Assertion({
                            label:asrtLabel,
                            args: args->Js_array2.map(n => tbl->addExprToProve(n->getExprFromNode))
                        })])
                    }
                }
            }
            None
        },
        ()
    )->ignore
    tbl
}

let printProofRec = (ctx,r) => {
    let exprStr = ctx->ctxExprToStr(r.expr)->Expln_utils_common.strJoin(~sep=" ", ())
    let proofs = switch r.src {
        | None => "no-proofs"
        | Some(proofs) => {
            let proofsLen = proofs->Js_array2.length
            if (r.proved && proofsLen == 1) {
                switch proofs[0] {
                    | Hypothesis({label}) => "hyp: " ++ label
                    | Assertion({args, label}) => args->Js_array2.map(Belt_Int.toString)->Expln_utils_common.strJoin(~sep=", ", ()) ++ " " ++ label
                }
            } else {
                Belt_Int.toString(proofsLen) ++ "-proofs"
            }
        }
    }
    let proved = if r.proved { "proved" } else { "not-proved" }
    `${proved} | ${proofs} | ${exprStr}`
}

let proofTablePrint = (ctx,tbl,title) => {
    Js.Console.log(`--- TBL ${title} ---------------------------------------------------------------------------`)
    tbl->Js_array2.map(printProofRec(ctx, _))->Js_array2.forEachi((str,i) => {
        Js.Console.log(`${Belt_Int.toString(i)}: ${str}`)
    })
    Js.Console.log("-----------------------------------------------------------------------------------")
}

let createOrderedProofTableFromProof: (mmContext, proofNode) => proofTable  = (ctx,proofNode) => {
    let processedExprs = Belt_MutableSet.make(~id = module(ExprCmp))
    let exprToIdx = Belt_MutableMap.make(~id = module(ExprCmp))
    let tbl = []
    Expln_utils_data.traverseTree(
        (),
        proofNode,
        (_,n) => {
            switch n {
                | Hypothesis(_) => None
                | Calculated({args,expr}) => {
                    if (processedExprs->Belt_MutableSet.has(expr)) {
                        None
                    } else {
                        Some(args)
                    }
                }
            }
        },
        ~process = (_, n) => {
            switch n {
                | Hypothesis({hypLabel,expr}) => {
                    if (exprToIdx->Belt_MutableMap.get(expr)->Belt_Option.isNone) {
                        let idx = tbl->Js_array2.push({dist:-1, proved:true, src:Some([Hypothesis({label:hypLabel})]), expr})-1
                        exprToIdx->Belt_MutableMap.set(expr,idx)
                    }
                }
                | _ => ()
            }
            None
        },
        ~postProcess = (_, n) => {
            switch n {
                | Calculated({args,asrtLabel,expr}) => {
                    if (exprToIdx->Belt_MutableMap.get(expr)->Belt_Option.isNone) {
                        let idx = tbl->Js_array2.push({
                            dist:-1,
                            proved:true,
                            src:Some([Assertion({
                                label:asrtLabel,
                                args: args->Js_array2.map(n => {
                                    let nExpr = getExprFromNode(n)
                                    exprToIdx->Belt_MutableMap.get(nExpr)->Belt_Option.getWithDefault(-1)
                                })
                            })]), 
                            expr
                        })-1
                        exprToIdx->Belt_MutableMap.set(expr,idx)
                    }
                }
                | _ => ()
            }
            None
        },
        ()
    )->ignore
    tbl
}