open Expln_utils_common
open MM_parser
open MM_context

type rec proofNode =
    | Hypothesis({hypLabel:string, expr:expr})
    | Calculated({args:array<proofNode>, asrtLabel:string, expr:expr})

let getExprFromNode = (node:proofNode):expr => {
    switch node {
        | Hypothesis({expr}) | Calculated({expr}) => expr
    }
}
let getExprFromStack = (stack:array<proofNode>, i:int):expr => getExprFromNode(stack[i])

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
        if (hyp.typ == F) {
            let t = hyp.expr[0]
            let v = hyp.expr[1]
            if (subsLock[v]) {
                raise(MmException({msg:`subsLock[v]`}))
            } else {
                let subsExpr = stack->getExprFromStack(baseIdx+i)
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
        if (hyp.typ == E && !compareExprAfterSubstitution(hyp.expr, subs, stack->getExprFromStack(baseIdx+i))) {
            raise(MmException({msg:`!compareExprAfterSubstitution(ess, subs, stack->getExprFromStack(baseIdx+i))`}))
        }
    })
}

let charCode = (str,pos) => str->Js.String2.codePointAt(pos)->Belt_Option.getExn
let charToInt = ch => charCode(ch, 0)
let zCode = charToInt("Z")
let aCode = charToInt("A")
let aCodePrev = aCode-1
let tCode = charToInt("T")
let uCode = charToInt("U")
let uCodePrev = uCode-1

let compressedProofBlockToArray = str => {
    let len = str->Js_string2.length
    let res = []
    let b = ref(0)
    let e = ref(0)
    while (e.contents < len) {
        let c = charCode(str,e.contents)
        if (c == zCode) {
            res->Js_array2.push("Z")->ignore
            e.contents=e.contents+1
            b.contents=e.contents
        } else if (aCode <= c && c <= tCode) {
            res->Js_array2.push(str->Js_string2.substring(~from=b.contents, ~to_=e.contents+1))->ignore
            e.contents=e.contents+1
            b.contents=e.contents
        } else {
            e.contents=e.contents+1
        }
    }
    res
}

let compressedProofCharCodeToInt = code => 
    if (code <= tCode) { code - aCodePrev } else { code - uCodePrev }

let compressedProofStrToInt = str => {
    let res = ref(0)
    let base = ref(1)
    let len = str->Js_string2.length
    for i in len-2 downto 0 {
        res.contents = res.contents + base.contents*compressedProofCharCodeToInt(charCode(str,i))
        base.contents = base.contents*5
    }
    res.contents*20 + compressedProofCharCodeToInt(charCode(str, len-1))
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
        for _ in 1 to frame.numOfArgs {
            stack->Js_array2.pop->ignore
        }
        stack->Js_array2.push(newNode)->ignore
    }
}

let applyUncompressedProof = (ctx, stack, proofLabels) => {
    proofLabels->Js_array2.forEach(step => {
        switch ctx->getHypothesis(step) {
            | Some(hyp) => stack->Js_array2.push(Hypothesis({hypLabel:hyp.label, expr:hyp.expr}))->ignore
            | None => {
                switch ctx->getFrame(step) {
                    | Some(frame) => applyAsrt(stack, frame)
                    | None => raise(MmException({msg:`The proofs step '${step}' doesn't refer to a hypothesis or assertion.`}))
                }
            }
        }
    })
}

let applyCompressedProof = (ctx, expr, stack, labels, compressedProofBlock) => {
    ()
    //let getHypLabel = (F(label,_) | E(label,_)) => label
    //let steps = compressedProofBlockToArray(compressedProofBlock)
    //let hyps = getMandHyps(ctx, expr)
    //let hypLen = hyps->Js_array2.length
    //let savedNodes = []
    //steps->Belt_Array.forEach(step => {
        //if (step == "Z") {
            //let stackLen = stack->Js_array2.length
            //if (stackLen == 0) {
                //raise(MmException({msg:`Cannot execute 'Z' command because the stack is empty`}))
            //} else {
                //savedNodes->Js_array2.push(stack[stackLen-1])->ignore
            //}
        //} else {
            //let i = compressedProofStrToInt(step)
            //if (1 <= i && i <= hypLen) {
                //let hyp = hyps[i-1]
                //stack->Js_array2.push({hypLabel:getHypLabel(hyp), expr:hypToExpr(hyp)})->ignore
            //} else if ()
        //}
    //})
}

let verifyProof: (mmContext, expr, proof) => proofNode = (ctx, expr, proof) => {
    let stack = []
    switch proof {
        | Compressed({labels, compressedProofBlock}) => applyCompressedProof(ctx, expr, stack, labels, compressedProofBlock)
        | Uncompressed({labels}) => applyUncompressedProof(ctx, stack, labels)
    }
    if (stack->Js_array2.length != 1) {
        raise(MmException({msg:`stack->Js_array2.length != 1`}))
    } else if (stack->getExprFromStack(0) != expr) {
        raise(MmException({msg:`stack[0] != expr`}))
    } else {
        stack[0]
    }
}


