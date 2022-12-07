open MM_context
open MM_substitution
open MM_proof_table
open MM_parser

let suggestPossibleProofs = (
    ~recToProve, 
    ~frms, 
    ~parenCnt, 
    ~tbl, 
    ~hyps:array<hypothesis>, 
    ~isDisjInCtx:(int,int) => bool
) => {
    let exprToProve = recToProve.expr
    let foundHyp = hyps->Expln_utils_common.arrForEach(hyp => if hyp.expr->exprEq(exprToProve) { Some(hyp) } else { None })
    switch foundHyp {
        | Some(hyp) => recToProve.branches = Some([Hypothesis({label:hyp.label})])
        | None => {
            let branches = []
            frms->Js_array2.forEach(frm => {
                if (frm.numOfHypsE == 0) {
                    iterateSubstitutions(
                        ~frmExpr = frm.frame.asrt,
                        ~expr = exprToProve,
                        ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                        ~constParts = frm.constParts[frm.numOfHypsE], 
                        ~varGroups = frm.varGroups[frm.numOfHypsE],
                        ~subs = frm.subs,
                        ~parenCnt,
                        ~consumer = subs => {
                            if (subs.isDefined->Js_array2.every(b=>b)) {
                                if (verifyDisjoints(~frmDisj=frm.frame.disj, ~subs, ~isDisjInCtx)) {
                                    let args: array<int> = frm.frame.hyps->Js_array2.map(hyp => {
                                        let exprToProve = applySubs(
                                            ~frmExpr = hyp.expr, 
                                            ~subs,
                                            ~createWorkVar = 
                                                _ => raise(MmException({msg:`Work variables are not supported in the syntax prover.`}))
                                        )
                                        tbl->addExprToProve(exprToProve)
                                    })
                                    branches->Js_array2.push(Assertion({
                                        args,
                                        label: frm.frame.label
                                    }))->ignore
                                }
                            }
                            Continue
                        }
                    )->ignore
                }
            })
            recToProve.branches = Some(branches)
        }
    }
}

let findProof = (
    ~frms, 
    ~parenCnt, 
    ~expr, 
    ~tbl, 
    ~hyps:array<hypothesis>, 
    ~isDisjInCtx:(int,int) => bool
) => {
    let targetIdx = tbl->addExprToProve(expr)
    tbl->markProved
    tbl->updateDist(targetIdx)
    let exprToProveIdx = ref(tbl->getNextExprToProveIdx)
    while (tbl[targetIdx].proof->Belt_Option.isNone && exprToProveIdx.contents->Belt_Option.isSome) {
        suggestPossibleProofs(~tbl, ~frms, ~parenCnt, ~hyps, ~isDisjInCtx,
            ~recToProve=tbl[exprToProveIdx.contents->Belt_Option.getExn]
        )
        tbl->markProved
        tbl->updateDist(targetIdx)
        exprToProveIdx.contents = tbl->getNextExprToProveIdx
    }
    targetIdx
}