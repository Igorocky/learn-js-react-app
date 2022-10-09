open MM_parser
open MM_context

type rec proofNode =
    | Hypothesis({hypLabel:string, expr:expr})
    | Calculated({args:array<proofNode>, asrtLabel:string, expr:expr})

let getExpr = (stack:array<proofNode>, i:int):expr => {
    switch stack[i] {
        | Hypothesis({expr}) | Calculated({expr}) => expr
    }
}

let compareSubArrays = (~src:array<'t>, ~srcFromIdx:int, ~dst:array<'t>, ~dstFromIdx:int, ~len:int): bool => {
    let s = ref(srcFromIdx)
    let d = ref(dstFromIdx)
    let srcLen = src->Js_array2.length
    let dstLen = dst->Js_array2.length
    if (srcLen < srcFromIdx+len || dstLen < dstFromIdx+len) {
        false
    } else {
        let sMax = srcFromIdx+len-1
        while (s.contents <= sMax && src[s.contents] == dst[d.contents]) {
            d.contents = d.contents + 1
            s.contents = s.contents + 1
        }
        s.contents > sMax
    }
}

let compareExprAfterSubstitution = (expr:expr, subs, eqTo:expr): bool => {
    let e = ref(0)
    let t = ref(0)
    let eq = ref(true)
    let eLen = expr->Js_array2.length
    let tLen = eqTo->Js_array2.length
    while (eq.contents && e.contents < eLen && t.contents < tLen) {
        let s = expr[e.contents]
        if (s < 0) {
            eq.contents = s == eqTo[t.contents]
            t.contents = t.contents + 1
        } else {
            let subExpr = subs[s]
            let len = subExpr->Js_array2.length-1
            eq.contents = compareSubArrays(~src=subExpr, ~srcFromIdx=1, ~dst=eqTo, ~dstFromIdx=t.contents, ~len)
            t.contents = t.contents + len
        }
        e.contents = e.contents + 1
    }
    eq.contents && e.contents == eLen && t.contents == tLen
}

let copySubArray = (~src:array<'t>, ~srcFromIdx:int, ~dst:array<'t>, ~dstFromIdx:int, ~len:int): unit => {
    let s = ref(srcFromIdx)
    let d = ref(dstFromIdx)
    let srcLen = src->Js_array2.length
    let dstLen = dst->Js_array2.length
    let sMax = Js_math.min_int(srcLen - 1, srcFromIdx + len - 1) 
    while (s.contents <= sMax && d.contents < dstLen) {
        dst[d.contents] = src[s.contents]
        d.contents = d.contents + 1
        s.contents = s.contents + 1
    }
}

let applySubs = (expr, subs): expr => {
    let resultSize = ref(0)
    expr->Js_array2.forEach(s => {
        if (s < 0) {
            resultSize.contents = resultSize.contents + 1
        } else {
            resultSize.contents = resultSize.contents + (subs[s]->Js_array2.length) - 1
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
            let subExpr = subs[s]
            let len = subExpr->Js_array2.length-1
            copySubArray(~src=subExpr, ~srcFromIdx=1, ~dst=res, ~dstFromIdx=r.contents, ~len)
            r.contents = r.contents + len
        }
        e.contents = e.contents + 1
    }
    res
}

let extractSubstitution = (stack:array<proofNode>, stackLength, frame):array<expr> => {
    let subs = Expln_utils_common.createArray(frame.numOfVars)
    let subsLock = Expln_utils_common.createArray(frame.numOfVars)->Js_array2.fillInPlace(false)
    let baseIdx = stackLength - frame.numOfArgs
    frame.hyps->Js_array2.forEachi((hyp,i) => {
        switch hyp {
            | F([t,v]) => {
                if (subsLock[v]) {
                    raise(MmException({msg:`subsLock[v]`}))
                } else {
                    let subsExpr = stack->getExpr(baseIdx+i)
                    if (subsExpr->Js_array2.length < 2) {
                        raise(MmException({msg:`subsExpr->Js_array2.length < 2`}))
                    } else if (subsExpr[0] != t) {
                        raise(MmException({msg:`subsExpr[0] != t`}))
                    } else {
                        subsLock[v] = true
                        subs[v] = subsExpr
                    }
                }
            }
            | _ => ()
        }
    })
    if (subsLock->Js_array2.some(lock => !lock)) {
        raise(MmException({msg:`subsLock->Js_array2.some(lock => !lock)`}))
    } else {
        subs
    }
}

let validateTopOfStackMatchesFrame = (stack:array<proofNode>, stackLength, frame, subs:array<expr>):unit => {
    let baseIdx = stackLength - frame.numOfArgs
    frame.hyps->Js_array2.forEachi((hyp,i) => {
        switch hyp {
            | E(ess) => {
                if (!compareExprAfterSubstitution(ess, subs, stack->getExpr(baseIdx+i))) {
                    raise(MmException({msg:`!compareExprAfterSubstitution(ess, subs, stack->getExpr(baseIdx+i))`}))
                }
            }
            | _ => ()
        }
    })
}

let applyAsrt = (stack:array<proofNode>, frame):unit => {
    let stackLength = stack->Js_array2.length
    if (stackLength < frame.numOfArgs) {
        raise(MmException({msg:`stackLength < numOfArgs`}))
    } else {
        let subs = extractSubstitution(stack, stackLength, frame)
        validateTopOfStackMatchesFrame(stack, stackLength, frame, subs)
        let newNode = Calculated({
            asrtLabel: frame.label,
            args: stack->Js_array2.sliceFrom(stackLength - frame.numOfArgs),
            expr: applySubs(frame.asrt, subs)
        })
        for i in 1 to frame.numOfArgs {
            let _ = stack->Js_array2.pop
        }
        let _ = stack->Js_array2.push(newNode)
    }
}

let verifyProof: (mmContext, expr, proof) => proofNode = (ctx, expr, proof) => {
    let stack = []
    switch proof {
        | Compressed(_) => raise(MmException({msg:`Verification of compressed proofs are to be implemented.`}))
        | Uncompressed({labels}) => {
            labels->Js_array2.forEach(step => {
                switch ctx->getHypothesisExpr(step) {
                    | Some(expr) => {
                        let _ = stack->Js_array2.push(Hypothesis({hypLabel:step, expr}))
                    }
                    | None => {
                        switch ctx->getFrame(step) {
                            | Some(frame) => {
                                applyAsrt(stack, frame)
                            }
                            | None => raise(MmException({msg:`The proofs step '${step}' doesn't refer to a hypothesis or assertion.`}))
                        }
                    }
                }
            })
        }
    }
    if (stack->Js_array2.length != 1) {
        raise(MmException({msg:`stack->Js_array2.length != 1`}))
    } else if (stack->getExpr(0) != expr) {
        raise(MmException({msg:`stack[0] != expr`}))
    } else {
        stack[0]
    }
}

