open MM_parser
open MM_context
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_proof_table
open MM_progress_tracker

type justification = {
    args: array<string>,
    asrt: string
}

type rootStmt = {
    label: string,
    expr: expr,
    justification: option<justification>,
}

type rec proofTreeNode = {
    expr:expr,
    label: option<string>,
    mutable parents: option<array<exprSource>>,
    mutable children: array<proofTreeNode>,
    mutable proof: option<exprSource>,
}
and exprSource =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<proofTreeNode>, label:string})

type proofTree = {
    frms: Belt_MapString.t<frmSubsData>,
    hypsByExpr: Belt_Map.t<expr,hypothesis,ExprCmp.identity>,
    hypsByLabel: Belt_MapString.t<hypothesis>,
    mutable maxVar:int,
    newVars: Belt_MutableSet.t<expr,ExprCmp.identity>,
    disj: disjMutable,
    parenCnt:parenCnt,
    nodes: Belt_MutableMap.t<expr,proofTreeNode,ExprCmp.identity>,
}

type proofTreeDto = {
    newVars: array<expr>,
    disj: disjMutable,
    nodes: array<proofTreeNode>,
}

let createEmptyProofTree = (
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~maxVar: int,
    ~disj: disjMutable,
    ~parenCnt:parenCnt,
) => {
    {
        frms,
        hypsByLabel: hyps,
        hypsByExpr: hyps->Belt_MapString.toArray->Js_array2.map(((_,hyp)) => (hyp.expr, hyp))->Belt_Map.fromArray(~id=module(ExprCmp)),
        maxVar,
        newVars: Belt_MutableSet.make(~id=module(ExprCmp)),
        disj,
        parenCnt,
        nodes: Belt_MutableMap.make(~id=module(ExprCmp)),
    }
}

let createOrUpdateNode = (
    ~tree:proofTree, 
    ~label:option<string>, 
    ~expr:expr, 
    ~child:option<proofTreeNode>
):proofTreeNode => {
    let result = switch tree.nodes->Belt_MutableMap.get(expr) {
        | Some(node) => node
        | None => {
            let node = {
                label,
                expr,
                parents: None,
                proof: None,
                children: [],
            }
            tree.nodes->Belt_MutableMap.set(expr, node)->ignore
            node
        }
    }
    switch child {
        | Some(child) => result.children->Js_array2.push(child)->ignore
        | None => ()
    }
    result
}

let markProved = ( node:proofTreeNode ) => {
    let getProofFromParents = (node):option<exprSource> => {
        switch node.parents {
            | None => None
            | Some(parents) => {
                parents->Expln_utils_common.arrForEach(parent => {
                    switch parent {
                        | VarType | Hypothesis(_) => Some(parent)
                        | Assertion({args}) => {
                            if (args->Js_array2.every(arg => arg.proof->Belt_Option.isSome)) {
                                Some(parent)
                            } else {
                                None
                            }
                        }
                    }
                })
            }
        }
    }

    switch getProofFromParents(node) {
        | None => ()
        | Some(nodeProof) => {
            node.proof = Some(nodeProof)
            let nodesToMarkProved = node.children->Belt_MutableQueue.fromArray
            let processedNodes = Belt_MutableSet.make(~id=module(ExprCmp))
            while (nodesToMarkProved->Belt_MutableQueue.size > 0) {
                let curNode = nodesToMarkProved->Belt_MutableQueue.pop->Belt_Option.getExn
                if (!(processedNodes->Belt_MutableSet.has(curNode.expr))) {
                    processedNodes->Belt_MutableSet.add(curNode.expr)
                    if (curNode.proof->Belt_Option.isNone) {
                        switch getProofFromParents(curNode) {
                            | None => ()
                            | Some(curNodeProof) => {
                                curNode.proof = Some(curNodeProof)
                                curNode.children->Js_array2.forEach( nodesToMarkProved->Belt_MutableQueue.add )
                            }
                        }
                    }
                }
            }
        }
    }
}

// let findMatchingParent = (
//     ~tree:proofTree,
//     ~node:proofTreeNode,
//     ~justification:justification
// ): option<exprSource> => {
//     switch node.parents {
//         | Some(parents) => {
//             parents->Js_array2.find(parent => {
//                 switch parent {
//                     | Assertion({args, label}) if label == justification.asrt => {
//                         let allArgsMatch = ref(true)
//                         let justifArgIdx = ref(0)
//                         switch tree.frms->Belt_MapString.get(label) {
//                             | None => raise(MmException({
//                                 msg:`Cannot find an assertion with label ${label} in findMatchingParent().`
//                             }))
//                             | Some(frm) => {
//                                 frm.frame.hyps->Js_array2.forEachi((hyp,hypIdx) => {
//                                     if (allArgsMatch.contents && hyp.typ == E) {
//                                         allArgsMatch.contents = 
//                                             args[hypIdx].label->Belt_Option.getWithDefault("") == justification.args[justifArgIdx.contents]
//                                         justifArgIdx.contents = justifArgIdx.contents + 1
//                                     }
//                                 })
//                             }
//                         }
//                         allArgsMatch.contents
//                     }
//                     | _ => false
//                 }
//             })
//         }
//         | None => None
//     }
// }

let getStatementsFromJustification = (
    ~tree:proofTree,
    ~stmts:array<labeledExpr>,
    ~justification: justification,
):array<labeledExpr> => {
    let getStmtByLabel = label => {
        switch stmts->Js.Array2.find(stmt => stmt.label == label) {
            | Some(stmt) => Some(stmt)
            | None => {
                switch tree.hypsByLabel->Belt_MapString.get(label) {
                    | Some(hyp) => Some({label, expr:hyp.expr})
                    | None => None
                }
            }
        }
    }
    let rootStmtsMap = Belt_MapString.fromArray(stmts->Js.Array2.map(({label,expr}) => (label, {label,expr})))
    let foundStmts = justification.args
        ->Js_array2.map(getStmtByLabel)
        ->Js_array2.filter(Belt_Option.isSome)
        ->Js_array2.map(Belt_Option.getExn)
    if (foundStmts->Js_array2.length == justification.args->Js.Array2.length) {
        foundStmts
    } else {
        []
    }
}

// let clearParents = node => {
//     switch node.parents {
//         | None => ()
//         | Some(parents) => {
//             parents->Js_array2.forEach(parent => {
//                 switch parent {
//                     | Assertion({args}) => {
//                         args->Js_array2.forEach(arg => {
//                             arg.children = arg.children->Js_array2.filter(child => child !== node)
//                         })
//                     }
//                     | _ => ()
//                 }
//             })
//             node.parents = None
//         }
//     }
// }

let rec proveNode = (
    ~tree:proofTree,
    ~stmts:array<labeledExpr>,
    ~node:proofTreeNode,
    ~justification: option<justification>,
) => {
    let rootNode = node
    let nodesToCreateParentsFor = Belt_MutableQueue.make()

    let addParentsWithoutNewVars = (node):unit => {
        tree.frms->Belt_MapString.forEach((_,frm) => {
            if (rootNode.proof->Belt.Option.isNone && node.proof->Belt.Option.isNone) {
                let frmExpr = frm.frame.asrt
                let expr = node.expr
                if (frmExpr[0] == expr[0]) {
                    iterateSubstitutions(
                        ~frmExpr,
                        ~expr,
                        ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                        ~constParts = frm.constParts[frm.numOfHypsE], 
                        ~varGroups = frm.varGroups[frm.numOfHypsE],
                        ~subs = frm.subs,
                        ~parenCnt=tree.parenCnt,
                        ~consumer = subs => {
                            if (subs.isDefined->Js_array2.every(b=>b)
                                && verifyDisjoints(~frmDisj=frm.frame.disj, ~subs, ~isDisjInCtx=tree.disj->disjContains)) {
                                let args = frm.frame.hyps->Js_array2.map(hyp => {
                                    let newExprToProve = applySubs(
                                        ~frmExpr = hyp.expr, 
                                        ~subs,
                                        ~createWorkVar = 
                                            _ => raise(MmException({msg:`Work variables are not supported in addParentsWithoutNewVars().`}))
                                    )
                                    let arg = createOrUpdateNode(
                                        ~tree, 
                                        ~label=None,
                                        ~expr=newExprToProve, 
                                        ~child=Some(node)
                                    )
                                    nodesToCreateParentsFor->Belt_MutableQueue.add(arg)
                                    arg
                                })
                                let parent = Assertion({ args, label:frm.frame.label })
                                node.parents->Belt.Option.getExn->Js_array2.push(parent)->ignore
                                markProved(node)
                            }
                            if (node.proof->Belt.Option.isNone) {
                                Continue
                            } else {
                                Stop
                            }
                        }
                    )->ignore
                }
            }
        })
    }

    let addParentsWithNewVars = (node,justification:option<justification>):unit => {
        let applResults = []
        switch justification {
            | None => {
                applyAssertions(
                    ~maxVar = tree.maxVar,
                    ~frms = tree.frms,
                    ~isDisjInCtx = tree.disj->disjContains,
                    ~statements = stmts,
                    ~result = node.expr,
                    ~parenCnt=tree.parenCnt,
                    ~onMatchFound = res => {
                        applResults->Js_array2.push(res)->ignore
                        Continue
                    },
                    ()
                )
            }
            | Some(justification) => {
                applyAssertions(
                    ~maxVar = tree.maxVar,
                    ~frms = tree.frms,
                    ~isDisjInCtx = tree.disj->disjContains,
                    ~statements = getStatementsFromJustification( ~tree, ~stmts, ~justification ),
                    ~exactOrderOfStmts=true,
                    ~result = node.expr,
                    ~parenCnt=tree.parenCnt,
                    ~frameFilter = frame => frame.label == justification.asrt,
                    ~onMatchFound = res => {
                        applResults->Js_array2.push(res)->ignore
                        Continue
                    },
                    ()
                )
            }
        }
        applResults->Expln_utils_common.arrForEach(applResult => {
            let applNewVarToTreeNewVar = Belt_MutableMapInt.make()
            applResult.newVars->Js.Array2.forEachi((v,i) => {
                tree.maxVar = tree.maxVar + 1
                let newVar = tree.maxVar
                applNewVarToTreeNewVar->Belt_MutableMapInt.set(v,newVar)
                let newVarType = applResult.newVarTypes[i]
                tree.newVars->Belt_MutableSet.add([newVarType, newVar])
            })
            applResult.newDisj->disjForEach((n,m) => {
                tree.disj->disjAddPair(
                    applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(n, n),
                    applNewVarToTreeNewVar->Belt_MutableMapInt.getWithDefault(m, m),
                )
            })
            let frame = switch tree.frms->Belt_MapString.get(applResult.asrtLabel) {
                | None => 
                    raise(MmException({msg:`Cannot find an assertion with label ${applResult.asrtLabel} in addParentsWithNewVars.`}))
                | Some(frm) => frm.frame
            }
            let args = frame.hyps->Js_array2.map(hyp => {
                createOrUpdateNode(
                    ~tree,
                    ~label=None,
                    ~expr = applySubs(
                        ~frmExpr = hyp.expr, 
                        ~subs=applResult.subs,
                        ~createWorkVar = _ => raise(MmException({msg:`New work variables are not expected here.`}))
                    ),
                    ~child=Some(node)
                )
            })
            if (checkTypes(~tree, ~frame, ~args)) {
                args->Js_array2.forEach(arg => {
                    if (arg.parents->Belt.Option.isNone) {
                        nodesToCreateParentsFor->Belt_MutableQueue.add(arg)
                    }
                })
                node.parents->Belt_Option.getExn->Js_array2.push(Assertion({
                    args,
                    label: applResult.asrtLabel
                }))->ignore
                markProved(node)
            }
            if (rootNode.proof->Belt.Option.isSome || node.proof->Belt.Option.isSome) {
                Some(true)
            } else {
                None
            }
        })->ignore
    }

    switch justification {
        | Some(justification) => {
            node.parents = Some([])
            addParentsWithNewVars(node,Some(justification))
        }
        | None => ()
    }

    nodesToCreateParentsFor->Belt_MutableQueue.add(node)
    while (rootNode.proof->Belt.Option.isNone && nodesToCreateParentsFor->Belt_MutableQueue.size > 0) {
        let curNode = nodesToCreateParentsFor->Belt_MutableQueue.pop->Belt_Option.getExn
        switch curNode.parents {
            | Some(_) => ()
            | None => {
                if (tree.newVars->Belt_MutableSet.has(curNode.expr)) {
                    curNode.parents = Some([VarType])
                    markProved(curNode)
                } else {
                    switch tree.hypsByExpr->Belt_Map.get(curNode.expr) {
                        | Some(hyp) => {
                            curNode.parents = Some([Hypothesis({label:hyp.label})])
                            markProved(curNode)
                        }
                        | None => {
                            curNode.parents = Some([])
                            addParentsWithoutNewVars(curNode)
                            if (rootNode.proof->Belt.Option.isNone 
                                    && curNode.proof->Belt.Option.isNone 
                                    && curNode.label->Belt_Option.isSome) {
                                addParentsWithNewVars(curNode, None)
                            }
                        }
                    }
                }
            }
        }
    }
}
and let checkTypes = (
    ~tree:proofTree,
    ~frame:frame,
    ~args:array<proofTreeNode>,
):bool => {
    let nodesToTypecheck = []
    frame.hyps->Js.Array2.forEachi((hyp,hypIdx) => {
        if (hyp.typ == F) {
            nodesToTypecheck->Js.Array2.push(args[hypIdx])->ignore
        }
    })
    nodesToTypecheck->Js.Array2.every(node => {
        proveNode(
            ~tree,
            ~stmts = [],
            ~node,
            ~justification=None,
        )
        node.proof->Belt_Option.isSome
    })
}

let proofTreeProve = (
    ~ctx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~stmts: array<rootStmt>,
    ~parenCnt: parenCnt,
    ~onProgress:option<float=>unit>=?,
    ()
):proofTree => {
    let stmtsProcessed = ref(0.)
    let progressState = ref(progressTrackerMake(~step=0.01, ~onProgress?, ()))

    let maxVar = ctx->getNumOfVars - 1
    let disj = ctx->getAllDisj
    let hyps = ctx->getAllHyps
    let tree = createEmptyProofTree(~frms, ~maxVar, ~disj, ~hyps, ~parenCnt, )
    for stmtIdx in 0 to stmts->Js_array2.length - 1 {
        let rootNode = createOrUpdateNode(
            ~tree, 
            ~label=Some(stmts[stmtIdx].label), 
            ~expr=stmts[stmtIdx].expr, 
            ~child=None
        )
        proveNode(
            ~tree,
            ~stmts=stmts->Js_array2.filteri((_,i) => i < stmtIdx)->Js_array2.map(({label,expr}) => {label,expr}),
            ~node=rootNode,
            ~justification=stmts[stmtIdx].justification,
        )
        stmtsProcessed.contents = stmtsProcessed.contents +. 1.
        progressState.contents = progressState.contents->progressTrackerSetCurrPct(
            stmtsProcessed.contents /. (stmtIdx + 1)->Belt_Int.toFloat
        )
    }
    tree
}

let proofTreeCreateProofTable = (node:proofTreeNode):proofTable => {
    let processedExprs = Belt_MutableSet.make(~id = module(ExprCmp))
    let exprToIdx = Belt_MutableMap.make(~id = module(ExprCmp))
    let tbl = []
    Expln_utils_data.traverseTree(
        (),
        node,
        (_,n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofTreeNode [1].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [1].`}))
                | Some(Hypothesis(_)) => None
                | Some(Assertion({args})) => {
                    if (processedExprs->Belt_MutableSet.has(n.expr)) {
                        None
                    } else {
                        Some(args)
                    }
                }
            }
        },
        ~process = (_, n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofTreeNode [2].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [2].`}))
                | Some(Hypothesis({label})) => {
                    if (exprToIdx->Belt_MutableMap.get(n.expr)->Belt_Option.isNone) {
                        let idx = tbl->Js_array2.push({proof:Hypothesis({label:label}), expr:n.expr})-1
                        exprToIdx->Belt_MutableMap.set(n.expr,idx)
                    }
                }
                | Some(Assertion(_)) => ()
            }
            None
        },
        ~postProcess = (_, n) => {
            switch n.proof {
                | None => raise(MmException({msg:`Cannot create proofTable from an unproved proofTreeNode [3].`}))
                | Some(VarType) => raise(MmException({msg:`VarType is not supported in createProofTable [3].`}))
                | Some(Hypothesis(_)) => ()
                | Some(Assertion({args,label})) => {
                    if (exprToIdx->Belt_MutableMap.get(n.expr)->Belt_Option.isNone) {
                        let idx = tbl->Js_array2.push({
                            proof:Assertion({
                                label:label,
                                args: args->Js_array2.map(n => {
                                    exprToIdx->Belt_MutableMap.get(n.expr)->Belt_Option.getWithDefault(-1)
                                })
                            }),
                            expr:n.expr
                        })-1
                        exprToIdx->Belt_MutableMap.set(n.expr,idx)
                    }
                }
            }
            None
        },
        ()
    )->ignore
    tbl
}