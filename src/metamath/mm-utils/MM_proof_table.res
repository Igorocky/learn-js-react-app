open MM_context
open MM_parser
open MM_proof_verifier

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    mutable proof:option<exprSource>,
    mutable dist:int,
    mutable branches: option<array<exprSource>>
}

type proofTable = array<proofRecord>

let printProofRec = (ctx,r) => {
    let exprStr = ctx->ctxExprToStrExn(r.expr)
    let proofs = switch r.proof {
        | Some(proof) => {
            switch proof {
                | Hypothesis({label}) => "hyp: " ++ label
                | Assertion({args, label}) => args->Js_array2.joinWith(", ") ++ " " ++ label
            }
        }
        | None => {
            switch r.branches {
                | None => "no-branches"
                | Some(branches) => Belt_Int.toString(branches->Js_array2.length) ++ "-branches"
            }
        }
    }
    let proved = if r.proof->Belt_Option.isSome { "proved" } else { "not-proved" }
    `${proved} | ${proofs} | ${exprStr}`
}

let proofTablePrint = (ctx,tbl,title) => {
    Js.Console.log(`--- TBL ${title} ---------------------------------------------------------------------------`)
    tbl->Js_array2.map(printProofRec(ctx, _))->Js_array2.forEachi((str,i) => {
        Js.Console.log(`${Belt_Int.toString(i)}: ${str}`)
    })
    Js.Console.log("-----------------------------------------------------------------------------------")
}

let getNextExprToProveIdx: proofTable => option<int> = tbl => {
    let minDist = ref(tbl->Js_array2.length)
    let res = ref(-1)
    let maxI = tbl->Js_array2.length-1
    for i in 0 to maxI {
        let r = tbl[i]
        if (r.proof->Belt_Option.isNone && r.branches->Belt.Option.isNone && r.dist < minDist.contents) {
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
                proof: None,
                dist: -1,
                branches: None
            })->ignore
            len
        }
    }
}

let markProved: proofTable => unit = tbl => {
    let provedIdxs = Belt_MutableSetInt.make()
    tbl->Js_array2.forEachi(({proof},i) => if proof->Belt_Option.isSome {provedIdxs->Belt_MutableSetInt.add(i)})

    let thereIsNewProvedExpr = ref(true)
    while (thereIsNewProvedExpr.contents) {
        thereIsNewProvedExpr.contents = false
        tbl->Js_array2.forEachi((proofRec,proofRecIdx) => {
            if (proofRec.proof->Belt_Option.isNone && proofRec.branches->Belt_Option.isSome) {
                let validProof = switch proofRec.branches {
                    | None => None
                    | Some(branches) => {
                        branches->Expln_utils_common.arrForEach(branch => {
                            switch branch {
                                | Hypothesis(_) => Some(branch)
                                | Assertion({args}) if args->Js_array2.every(provedIdxs->Belt_MutableSetInt.has) => Some(branch)
                                | _ => None
                            }
                        })
                    }
                }
                switch validProof {
                    | None => ()
                    | Some(_) => {
                        proofRec.branches = None
                        proofRec.proof = validProof
                        thereIsNewProvedExpr.contents = true
                        provedIdxs->Belt_MutableSetInt.add(proofRecIdx)
                    }
                }
            }
        })
    }
}

let updateDist: (proofTable,int) => unit = (tbl,targetIdx) => {
    let tblLen = tbl->Js_array2.length
    tbl->Js_array2.forEach(r => r.dist=tblLen)
    tbl[targetIdx].dist = 0
    let queue = Belt_MutableQueue.make()
    queue->Belt_MutableQueue.add(tbl[targetIdx])
    while (!(queue->Belt_MutableQueue.isEmpty)) {
        let parent = queue->Belt_MutableQueue.pop->Belt_Option.getExn
        let childDist = parent.dist+1
        switch parent.branches {
            | None => ()
            | Some(branches) => {
                branches->Js_array2.forEach(branch => {
                    switch branch {
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

let traverseRecordsInRpnOrder = (tbl,targetIdx,~onUse,~onReuse) => {
    let savedExprs = Belt_MutableSet.make(~id=module(ExprCmp))
    let reusedExprsSet = Belt_MutableSet.make(~id=module(ExprCmp))
    Expln_utils_data.traverseTree(
        (),
        tbl[targetIdx],
        (_, r) => {
            switch r.proof {
                | None => raise(MmException({msg: `Unexpected condition: a proof table record to be used for proof generation doesn't have a proof.`}))
                | Some(Hypothesis(_)) => None
                | Some(Assertion({args})) => if (savedExprs->Belt_MutableSet.has(r.expr)) { None } else { Some(args->Js_array2.map(a=>tbl[a])) }
            }
        },
        ~process = (_, r) => {
            switch r.proof {
                | Some(Hypothesis(_)) => onUse(r)
                | _ => ()
            }
            None
        },
        ~postProcess = (_, r) => {
            switch r.proof {
                | Some(Assertion(_)) => {
                    if (!(savedExprs->Belt_MutableSet.has(r.expr))) {
                        savedExprs->Belt_MutableSet.add(r.expr)
                        onUse(r)
                    } else {
                        let firstReusage = !(reusedExprsSet->Belt_MutableSet.has(r.expr))
                        if (firstReusage) {
                            reusedExprsSet->Belt_MutableSet.add(r.expr)
                        }
                        onReuse(r,firstReusage)
                    }
                }
                | _ => ()
            }
            None
        },
        ()
    )->ignore
}

let collectReusedExprs = (tbl,targetIdx):Belt_Set.t<expr, ExprCmp.identity> => {
    let reusedExprs = []
    traverseRecordsInRpnOrder(tbl,targetIdx,
        ~onUse = _ => (),
        ~onReuse = (r,firstReusage) => {
            if (firstReusage) {
                reusedExprs->Js_array2.push(r.expr)->ignore
            }
        }
    )
    Belt_Set.fromArray(reusedExprs, ~id=module(ExprCmp))
}

let createProof = (ctx:mmContext, tbl:proofTable, targetIdx:int):proof => {
    let tblLen = tbl->Js_array2.length
    if (tblLen <= targetIdx) {
        raise(MmException({msg:`tblLen <= targetIdx`}))
    }
    if (tbl[targetIdx].proof->Belt_Option.isNone) {
        raise(MmException({msg:`The target record in a proofTable must have a proof.`}))
    }
    let mandHyps = getMandHyps(ctx, tbl[targetIdx].expr)
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
    let reusedExprs = collectReusedExprs(tbl,targetIdx)
    let reusedExprToInt = Belt_MutableMap.make(~id=module(ExprCmp))
    let proofSteps = []
    traverseRecordsInRpnOrder(tbl,targetIdx,
        ~onUse = r => {
            let idx = switch r.proof {
                | Some(Hypothesis({label})) => {
                    switch mandHypLabelToInt->Belt_MapString.get(label) {
                        | Some(i) => i
                        | None => labelToInt(label)
                    }
                }
                | Some(Assertion({label})) => labelToInt(label)
                | _ => raise(MmException({msg:`Cannot determine index of a proof step.`}))
            }
            proofSteps->Js_array2.push(idx)->ignore
            if (reusedExprs->Belt_Set.has(r.expr)) {
                proofSteps->Js_array2.push(0)->ignore
                reusedExprToInt->Belt_MutableMap.set(r.expr, reusedExprToInt->Belt_MutableMap.size + 1)
            }
        },
        ~onReuse = (r,_) => {
            proofSteps->Js_array2.push(-(reusedExprToInt->Belt_MutableMap.getExn(r.expr)))->ignore
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
        })->Js_array2.joinWith("")
    })

}

let createOrderedProofTableFromProof: proofNode => proofTable  = proofNode => {
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
                        let idx = tbl->Js_array2.push({dist:-1, proof:Some(Hypothesis({label:hypLabel})), branches:None, expr})-1
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
                            proof:Some(Assertion({
                                label:asrtLabel,
                                args: args->Js_array2.map(n => {
                                    let nExpr = getExprFromNode(n)
                                    exprToIdx->Belt_MutableMap.get(nExpr)->Belt_Option.getWithDefault(-1)
                                })
                            })),
                            branches:None, 
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