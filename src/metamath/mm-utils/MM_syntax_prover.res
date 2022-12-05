open MM_context
open MM_substitution
open MM_proof_table
open MM_parser

let suggestPossibleProofs = (~recToProve, ~frameData, ~parenCnt, ~tbl, ~ctx) => {
    let exprToProve = recToProve.expr
    let foundHyp = ctx->forEachHypothesisInDeclarationOrder(hyp => if hyp.expr->exprEq(exprToProve) { Some(hyp) } else { None })
    switch foundHyp {
        | Some(hyp) => recToProve.branches = Some([Hypothesis({label:hyp.label})])
        | None => {
            let branches = []
            frameData->Js_array2.forEach(frm => {
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
                            Continue
                        }
                    )
                }
            })
            recToProve.branches = Some(branches)
        }
    }
}

let findProof = (~ctx, ~frameProofData as frameData, ~parenCnt, ~expr, ~proofTbl as tbl) => {
    let targetIdx = tbl->addExprToProve(expr)
    tbl->markProved
    tbl->updateDist(targetIdx)
    let exprToProveIdx = ref(tbl->getNextExprToProveIdx)
    while (tbl[targetIdx].proof->Belt_Option.isNone && exprToProveIdx.contents->Belt_Option.isSome) {
        suggestPossibleProofs(~tbl, ~ctx, ~frameData, ~parenCnt,
            ~recToProve=tbl[exprToProveIdx.contents->Belt_Option.getExn]
        )
        tbl->markProved
        tbl->updateDist(targetIdx)
        exprToProveIdx.contents = tbl->getNextExprToProveIdx
    }
    targetIdx
}