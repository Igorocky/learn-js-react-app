open MM_parser
open MM_context
open MM_proof_verifier
open MM_substitution
open MM_asrt_apply
open MM_parenCounter
open MM_proof_table

type rootStmt = {
    label: string,
    expr: expr,
    justification: option<array<string>>,
}

type rec proofTreeNode = {
    expr:expr,
    mutable parents: option<array<exprSource>>,
    children: array<proofTreeNode>,
    mutable proof: option<exprSource>,
    mutable dist: int,
    mutable syntax: option<proofTreeNode>,
}
and exprSource =
    | VarType
    | Hypothesis({label:string})
    | Assertion({args:array<proofTreeNode>, label:string})

type proofTree = {
    parenCnt:parenCnt,
    frms: Belt_MapString.t<frmSubsData>,
    hyps: Belt_MutableMap.t<expr,hypothesis,ExprCmp.identity>,
    newVars: Belt_MutableSet.t<expr,ExprCmp.identity>,
    newVarTypes: Belt_MutableMapInt.t<int>,
    mutable maxVar:int,
    disj: disjMutable,
    nodes: Belt_MutableMap.t<expr,proofTreeNode,ExprCmp.identity>,
}

let createEmptyProofTree = (
    ~parenCnt:parenCnt,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~maxVar: int,
    ~disj: disjMutable,
) => {
    let hypsMap = Belt_MutableMap.make(~id=module(ExprCmp))
    hyps->Belt_MapString.forEach((k,v) => {
        hypsMap->Belt_MutableMap.set(v.expr, v)
    })
    {
        parenCnt,
        frms,
        hyps: hypsMap,
        newVars: Belt_MutableSet.make(~id=module(ExprCmp)),
        newVarTypes: Belt_MutableMapInt.make(),
        maxVar,
        disj,
        nodes: Belt_MutableMap.make(~id=module(ExprCmp))
    }
}

let addExprToProve = (~tree:proofTree, ~expr:expr, ~dist:int, ~child:option<proofTreeNode>):proofTreeNode => {
    let result = switch tree.nodes->Belt_MutableMap.get(expr) {
        | Some(node) => node
        | None => {
            let node = {
                expr,
                parents: None,
                children: [],
                proof: None,
                dist,
                syntax: None,
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
                parents->Expln_utils_common.arrForEach(src => {
                    switch src {
                        | VarType | Hypothesis(_) => {
                            Some(src)
                        }
                        | Assertion({args}) => {
                            if (args->Js_array2.every(arg => arg.proof->Belt_Option.isSome)) {
                                Some(src)
                            } else {
                                None
                            }
                        }
                    }
                })
            }
        }
    }

    let nodesToMarkProved = switch getProofFromParents(node) {
        | None => None
        | Some(src) => {
            node.proof = Some(src)
            Some(node.children->Belt_MutableQueue.fromArray)
        }
    }
    while (nodesToMarkProved->Belt_Option.isSome 
                && nodesToMarkProved->Belt_Option.getExn->Belt_MutableQueue.size > 0) {
        let curNode = nodesToMarkProved->Belt_Option.getExn->Belt_MutableQueue.pop->Belt_Option.getExn
        if (curNode.proof->Belt_Option.isNone) {
            switch getProofFromParents(curNode) {
                | None => ()
                | Some(src) => {
                    curNode.proof = Some(src)
                    curNode.children->Js_array2.forEach(
                        nodesToMarkProved->Belt_Option.getExn->Belt_MutableQueue.add
                    )
                }
            }
        }
    }
}

let proveStmt = (
    ~tree:proofTree,
    ~stmts:array<rootStmt>,
    ~stmtIdx:int,
    ~searchDepth:int,
) => {
    let nodesToCreateParentsFor = Belt_MutableQueue.fromArray([addExprToProve(
        ~tree, ~expr=stmts[stmtIdx].expr, ~dist=0, ~child=None
    )])

    let addParentsWithoutNewVars = (node) => {
        tree.frms->Belt_MapString.forEach((_,frm) => {
            if (node.proof->Belt.Option.isNone) {
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
                                    let arg = addExprToProve(~tree, ~expr=newExprToProve, ~dist=node.dist+1, ~child=Some(node))
                                    nodesToCreateParentsFor->Belt_MutableQueue.add(arg)
                                    arg
                                })
                                node.parents->Belt_Option.getExn->Js_array2.push(Assertion({
                                    args,
                                    label: frm.frame.label
                                }))->ignore
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

    let addParentsWithNewVars = node => {
        //to be implemented
        ()
    }
    
    while (nodesToCreateParentsFor->Belt_MutableQueue.size > 0) {
        let curNode = nodesToCreateParentsFor->Belt_MutableQueue.pop->Belt_Option.getExn
        switch curNode.parents {
            | Some(_) => ()
            | None => {
                if (tree.newVars->Belt_MutableSet.has(curNode.expr)) {
                    curNode.parents = Some([VarType])
                    markProved(curNode)
                } else {
                    let foundHyp = tree.hyps->Belt_MutableMap.get(curNode.expr)
                    switch foundHyp {
                        | Some(hyp) => {
                            curNode.parents = Some([Hypothesis({label:hyp.label})])
                            markProved(curNode)
                        }
                        | None => {
                            curNode.parents = Some([])
                            addParentsWithoutNewVars(curNode)
                            if (curNode.proof->Belt.Option.isNone && searchDepth <= curNode.dist) {
                                addParentsWithNewVars(curNode)
                            }
                        }
                    }
                }
            }
        }
    }
}

let proofTreeProve = (
    ~parenCnt: parenCnt,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~hyps: Belt_MapString.t<hypothesis>,
    ~maxVar: int,
    ~disj: disjMutable,
    ~stmts: array<rootStmt>,
    ~searchDepth: int,
):proofTree => {
    let tree = createEmptyProofTree(~parenCnt, ~frms, ~hyps, ~maxVar, ~disj)
    for i in 0 to stmts->Js_array2.length - 1 {
        proveStmt(
            ~tree,
            ~stmts,
            ~stmtIdx=i,
            ~searchDepth,
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