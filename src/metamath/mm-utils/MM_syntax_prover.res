open MM_context
open MM_substitution
open MM_proof_table

let applySubs = (expr, subs): expr => {
    let resultSize = ref(0)
    expr->Js_array2.forEach(s => {
        if (s < 0) {
            resultSize.contents = resultSize.contents + 1
        } else {
            resultSize.contents = resultSize.contents + (subs.ends[s]-subs.begins[s]+1)
        }
    })
    let res = Expln_utils_common.createArray(resultSize.contents)
    let e = ref(0)
    let r = ref(0)
    while (r.contents < resultSize.contents) {
        let s = expr[e.contents]
        if (s < 0) {
            res[r.contents] = s
            r.contents = r.contents + 1
        } else {
            let subExpr = subs.exprs[s]
            let len = subExpr->Js_array2.length
            Expln_utils_common.copySubArray(~src=subExpr, ~srcFromIdx=subs.begins[s], ~dst=res, ~dstFromIdx=r.contents, ~len)
            r.contents = r.contents + len
        }
        e.contents = e.contents + 1
    }
    res
}

let suggestPossibleProofs = (~recToProve, ~frameData, ~parenCnt, ~tbl, ~ctx) => {
    let exprToProve = recToProve.expr
    let foundHyp = ctx->forEachHypothesisInDeclarationOrder(hyp => if hyp.expr->exprEq(exprToProve) { Some(hyp) } else { None })
    switch foundHyp {
        | Some(hyp) => recToProve.branches = Some([Hypothesis({label:hyp.label})])
        | None => {
            let branches = []
            frameData->Js_array2.forEach(frmData => {
                if (frmData.hypsE->Js.Array2.length == 0) {
                    iterateSubstitutions(
                        ~frmExpr = frmData.frame.asrt,
                        ~expr = exprToProve,
                        ~frmConstParts = frmData.frmConstParts, 
                        ~constParts = frmData.constParts, 
                        ~varGroups = frmData.varGroups,
                        ~subs = frmData.subs,
                        ~parenCnt,
                        ~consumer = subs => {
                            if (subs.isDefined->Js_array2.every(b=>b)) {
                                let args: array<int> = frmData.frame.hyps->Js_array2.map(hyp => tbl->addExprToProve(applySubs(hyp.expr, subs)))
                                branches->Js_array2.push(Assertion({
                                    args,
                                    label: frmData.frame.label
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