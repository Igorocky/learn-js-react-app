open Expln_utils_common
open MM_parenCounter
open MM_context

type contunieInstruction = Continue | Stop

type constParts = {
    length: int,
    begins: array<int>,
    ends: array<int>,
    remainingMinLength: array<int>,
}

type varGroup = {
    leftConstPartIdx:int,
    frmExpr:expr,
    varsBeginIdx:int,
    numOfVars:int,
    mutable exprBeginIdx:int,
    mutable exprEndIdx:int
}

type subs = {
    size: int,
    begins: array<int>,
    ends: array<int>,
    exprs: array<expr>,
    isDefined: array<bool>,
}

let lengthOfGap = (leftConstPartIdx:int, constParts:array<array<int>>, exprLength:int):int => {
    if (leftConstPartIdx < 0) {
        constParts[0][0]
    } else if (leftConstPartIdx < constParts->Js_array2.length - 1) {
        constParts[leftConstPartIdx+1][0] - constParts[leftConstPartIdx][1] - 1
    } else {
        exprLength - constParts[leftConstPartIdx][1] - 1
    }
}

let lengthOfGap2 = (leftConstPartIdx:int, constParts:constParts, exprLength:int):int => {
    if (leftConstPartIdx < 0) {
        constParts.begins[0]
    } else if (leftConstPartIdx < constParts.length - 1) {
        constParts.begins[leftConstPartIdx+1] - constParts.ends[leftConstPartIdx] - 1
    } else {
        exprLength - constParts.ends[leftConstPartIdx] - 1
    }
}

let createConstParts = expr => {
    let constParts = []
    for i in 0 to expr->Js_array2.length-1 {
        let constPartsLength = constParts->Js_array2.length
        if (expr[i] < 0) {
            if (constPartsLength == 0 || constParts[constPartsLength-1][1] >= 0) {
                constParts->Js_array2.push([i,-1])->ignore
            }
        } else if (constPartsLength > 0 && constParts[constPartsLength-1][1] < 0) {
            constParts[constPartsLength-1][1] = i-1
        }
    }
    let constPartsLength = constParts->Js_array2.length
    let exprLength = expr->Js_array2.length
    if (constPartsLength > 0 && constParts[constPartsLength-1][1] < 0) {
        constParts[constPartsLength-1][1] = exprLength-1
    }
    let result = {
        length: constPartsLength,
        begins: createArray(constPartsLength),
        ends: createArray(constPartsLength),
        remainingMinLength: createArray(constPartsLength)
    }
    let remainingMinLength = ref(0)
    for i in constPartsLength-1 downto 0 {
        result.begins[i] = constParts[i][0]
        result.ends[i] = constParts[i][1]
        remainingMinLength.contents = remainingMinLength.contents + (result.ends[i] - result.begins[i] + 1) + lengthOfGap(i, constParts, exprLength)
        result.remainingMinLength[i] = remainingMinLength.contents
    }
    result
}

let createMatchingConstParts = constParts => {
    {
        length: constParts.length,
        begins: createArray(constParts.length),
        ends: createArray(constParts.length),
        remainingMinLength: []
    }
}

let rec iterateConstParts = (
    ~frmExpr:expr, 
    ~expr:expr, 
    ~frmConstParts:constParts, 
    ~constParts:constParts, 
    ~idxToMatch:int, 
    ~parenCnt:parenCnt,
    ~consumer:constParts => contunieInstruction
):contunieInstruction => {
    let invokeNext = ():contunieInstruction => {
        iterateConstParts(
            ~frmExpr, 
            ~expr, 
            ~frmConstParts, 
            ~constParts, 
            ~idxToMatch=idxToMatch+1, 
            ~parenCnt,
            ~consumer
        )
    }

    let exprLen = expr->Js_array2.length
    let frmExprLen = frmExpr->Js_array2.length

    if (idxToMatch == frmConstParts.length) {
        if (frmConstParts.length > 0 && constParts.ends[idxToMatch-1] != exprLen-1) {
            if (frmConstParts.ends[idxToMatch-1] == frmExprLen-1) {
                Continue
            } else {
                let frmRemainingGapLength = lengthOfGap2(idxToMatch-1, frmConstParts, frmExprLen)
                let remainingGapLength = lengthOfGap2(idxToMatch-1, constParts, exprLen)
                if (remainingGapLength < frmRemainingGapLength) {
                    Continue
                } else {
                    parenCnt->parenCntReset
                    let pState = ref(Balanced)
                    let i = ref(constParts.ends[idxToMatch-1]+1)
                    while (i.contents < exprLen && pState.contents != Failed) {
                        pState.contents = parenCnt->parenCntPut(expr[i.contents])
                        i.contents = i.contents + 1
                    }
                    if (pState.contents == Balanced) {
                        consumer(constParts)
                    } else {
                        Continue
                    }
                }
            }
        } else {
            consumer(constParts)
        }
    } else if (idxToMatch == 0 && frmConstParts.begins[0] == 0) {
        if (exprLen-1 < frmConstParts.ends[0]) {
            Continue
        } else {
            let res = ref(None)
            let maxI = frmConstParts.ends[0]
            let i = ref(0)
            while (res.contents->Belt_Option.isNone && i.contents <= maxI) {
                if (frmExpr[i.contents] != expr[i.contents]) {
                    res.contents = Some(Continue)
                }
                i.contents = i.contents + 1
            }
            switch res.contents {
                | Some(instr) => instr
                | None => {
                    constParts.begins[0] = 0
                    constParts.ends[0] = maxI
                    invokeNext()
                }
            }
        }
    } else {
        let begin = ref(if (idxToMatch == 0) {0} else {constParts.ends[idxToMatch-1]+1})
        let maxBegin = exprLen - frmConstParts.remainingMinLength[idxToMatch]
        parenCnt->parenCntReset
        let pState = ref(Balanced)
        let numOfVars = lengthOfGap2(idxToMatch-1,frmConstParts,frmExprLen)
        for _ in 1 to numOfVars {
            pState.contents = parenCnt->parenCntPut(expr[begin.contents])
            begin.contents = begin.contents + 1
        }
        let partLen = frmConstParts.ends[idxToMatch] - frmConstParts.begins[idxToMatch] + 1
        let instr = ref(Continue)
        while (begin.contents <= maxBegin && pState.contents != Failed && instr.contents == Continue) {
            if (pState.contents == Balanced) {
                let matchedLen = ref(0)
                let cmpRes = ref(true)
                while (matchedLen.contents < partLen && cmpRes.contents) {
                    cmpRes.contents = frmExpr[frmConstParts.begins[idxToMatch]+matchedLen.contents] == expr[begin.contents+matchedLen.contents]
                    matchedLen.contents = matchedLen.contents + 1
                }
                if (matchedLen.contents == partLen && cmpRes.contents) {
                    constParts.begins[idxToMatch] = begin.contents
                    constParts.ends[idxToMatch] = begin.contents+partLen-1
                    instr.contents = invokeNext()
                    parenCnt->parenCntReset
                }
            }
            pState.contents = parenCnt->parenCntPut(expr[begin.contents])
            begin.contents = begin.contents + 1
        }
        instr.contents
    }
}

let createVarGroups = (~frmExpr:expr, ~frmConstParts:constParts): array<varGroup> => {
    let frmExprLen = frmExpr->Js_array2.length
    if (frmConstParts.length == 0) {
        [{
            leftConstPartIdx: -1,
            frmExpr:frmExpr,
            varsBeginIdx: 0,
            numOfVars: frmExprLen,
            exprBeginIdx: 0,
            exprEndIdx: 0
        }]
    } else {
        let res = []
        if (frmConstParts.begins[0] != 0) {
            res->Js_array2.push({
                leftConstPartIdx: -1,
                frmExpr:frmExpr,
                varsBeginIdx:0,
                numOfVars:frmConstParts.begins[0],
                exprBeginIdx: 0,
                exprEndIdx: 0
            })->ignore
        }
        for i in 0 to frmConstParts.length-2 {
            res->Js_array2.push({
                leftConstPartIdx: i,
                frmExpr:frmExpr,
                varsBeginIdx: frmConstParts.ends[i]+1,
                numOfVars: lengthOfGap2(i, frmConstParts, frmExprLen),
                exprBeginIdx: 0,
                exprEndIdx: 0
            })->ignore
        }
        let lastConstPartIdx = frmConstParts.length-1
        if (frmConstParts.ends[lastConstPartIdx] != frmExprLen-1) {
            res->Js_array2.push({
                leftConstPartIdx: lastConstPartIdx,
                frmExpr:frmExpr,
                varsBeginIdx: frmConstParts.ends[lastConstPartIdx]+1,
                numOfVars: lengthOfGap2(lastConstPartIdx, frmConstParts, frmExprLen),
                exprBeginIdx: 0,
                exprEndIdx: 0
            })->ignore
        }
        res
    }
}

let initVarGroups = (~varGroups:array<varGroup>, ~constParts:constParts, ~expr:expr) => {
    let exprLen = expr->Js_array2.length
    if (constParts.length == 0) {
        varGroups[0].exprBeginIdx = 0
        varGroups[0].exprEndIdx = exprLen-1
    } else {
        varGroups->Js_array2.forEach(grp => {
            if (grp.leftConstPartIdx == -1) {
                grp.exprBeginIdx = 0
                grp.exprEndIdx = constParts.begins[0] - 1
            } else if (grp.leftConstPartIdx == constParts.length-1) {
                grp.exprBeginIdx = constParts.ends[grp.leftConstPartIdx]+1
                grp.exprEndIdx = exprLen-1
            } else {
                grp.exprBeginIdx = constParts.ends[grp.leftConstPartIdx]+1
                grp.exprEndIdx = constParts.begins[grp.leftConstPartIdx+1]-1
            }
        })
        varGroups->Js.Array2.sortInPlaceWith((g1,g2) => {
            if (g1.numOfVars < g2.numOfVars) {
                -1
            } else if (g1.numOfVars == g2.numOfVars) {
                0
            } else {
                1
            }
        })->ignore
    }
}

let rec iterateVarGroups = (
    ~expr:expr,
    ~subs:subs,
    ~varGroups:array<varGroup>,
    ~curGrpIdx:int,
    ~curVarIdx:int,
    ~subExprBeginIdx:int,
    ~parenCnt:parenCnt,
    ~consumer: subs=>contunieInstruction
): contunieInstruction => {
    let grp = varGroups[curGrpIdx]
    let varNum = grp.frmExpr[grp.varsBeginIdx+curVarIdx]
    let maxSubExprLength = grp.exprEndIdx - subExprBeginIdx + 1 - (grp.numOfVars - curVarIdx - 1)
    
    let invokeNext = (subExprLength:int):contunieInstruction => {
        if (curVarIdx < grp.numOfVars - 1) {
            iterateVarGroups(
                ~expr,
                ~subs,
                ~varGroups,
                ~curGrpIdx,
                ~curVarIdx = curVarIdx+1,
                ~subExprBeginIdx = subExprBeginIdx+subExprLength,
                ~parenCnt,
                ~consumer
            )
        } else if (curGrpIdx < varGroups->Js_array2.length-1) {
            iterateVarGroups(
                ~expr,
                ~subs,
                ~varGroups,
                ~curGrpIdx = curGrpIdx+1,
                ~curVarIdx = 0,
                ~subExprBeginIdx = varGroups[curGrpIdx+1].exprBeginIdx,
                ~parenCnt,
                ~consumer
            )
        } else {
            consumer(subs)
        }
    }

    let continueInstr = ref(Continue)
    if (!subs.isDefined[varNum]) {
        subs.isDefined[varNum] = true
        subs.exprs[varNum] = expr
        subs.begins[varNum] = subExprBeginIdx
        if (curVarIdx == grp.numOfVars-1) {
            subs.ends[varNum] = grp.exprEndIdx
            continueInstr.contents = invokeNext(maxSubExprLength)
        } else {
            let subExprLength = ref(1)
            let end = ref(subExprBeginIdx)
            parenCnt->parenCntReset
            let pStatus = ref(Balanced)
            while (subExprLength.contents <= maxSubExprLength && continueInstr.contents == Continue && pStatus.contents != Failed) {
                subs.ends[varNum] = end.contents
                pStatus.contents = parenCnt->parenCntPut(expr[end.contents])
                if (pStatus.contents == Balanced) {
                    continueInstr.contents = invokeNext(subExprLength.contents)
                    parenCnt->parenCntReset
                }
                subExprLength.contents = subExprLength.contents + 1
                end.contents = end.contents + 1
            }
        }
        subs.isDefined[varNum] = false
    } else {
        let existingExpr = subs.exprs[varNum]
        let existingExprBeginIdx = subs.begins[varNum]
        let existingExprLen = subs.ends[varNum] - existingExprBeginIdx + 1
        if (existingExprLen <= maxSubExprLength && (varNum < grp.numOfVars-1 || existingExprLen == maxSubExprLength)) {
            let checkedLen = ref(0)
            while (checkedLen.contents < existingExprLen 
                    && existingExpr[existingExprBeginIdx+checkedLen.contents] == expr[subExprBeginIdx+checkedLen.contents]) {
                        checkedLen.contents = checkedLen.contents + 1
            }
            if (checkedLen.contents == existingExprLen) {
                continueInstr.contents = invokeNext(existingExprLen)
            }
        }
    }
    continueInstr.contents
}

let iterateSubstitutions = (
    ~frmExpr:expr, 
    ~expr:expr, 
    ~frmConstParts:constParts, 
    ~constParts:constParts, 
    ~varGroups:array<varGroup>,
    ~subs:subs,
    ~parenCnt:parenCnt,
    ~consumer: subs => contunieInstruction
) => {
    if (subs.size == 0) {
        if (frmExpr == expr) {
            consumer(subs)->ignore
        }
    } else {
        iterateConstParts(
            ~frmExpr, 
            ~expr, 
            ~frmConstParts, 
            ~constParts, 
            ~idxToMatch=0,
            ~parenCnt,
            ~consumer = constParts => {
                initVarGroups(~varGroups, ~constParts, ~expr)
                for i in 0 to subs.size-1 {
                    subs.isDefined[i] = false
                }
                iterateVarGroups(
                    ~expr,
                    ~subs,
                    ~varGroups,
                    ~curGrpIdx = 0,
                    ~curVarIdx = 0,
                    ~subExprBeginIdx = varGroups[0].exprBeginIdx,
                    ~parenCnt,
                    ~consumer
                )
            }
        )->ignore
    }
}

let createSubs: (~numOfVars:int) => subs = (~numOfVars) => {
    {
        size: numOfVars,
        begins: Belt_Array.make(numOfVars, 0),
        ends: Belt_Array.make(numOfVars, 0),
        exprs: Belt_Array.make(numOfVars, []),
        isDefined: Belt_Array.make(numOfVars, false),
    }
}

//let numberOfStates = (numOfVars, subExprLength) => {
    //let n = subExprLen - 1
    //let k = numOfVars - 1

    //let res = ref(1)
    //let rem = ref(2)
    //let minI = n-k+1
    //for i in minI to n {
        //res.contents = Js.Math.imul(res.contents, i)
    //}
//}

//let numberOfStates = varGroup => {
    //let subExprLen = varGroup.exprEndIdx-varGroup.exprBeginIdx+1
    //let n = subExprLen - 1
    //let k = 
//}

//let iterateSubs: (~expr:expr, ~frmExpr:expr, ~frame:frame, ~consumer:subs=>contunieInstruction) => unit

let test_iterateConstParts: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr) => (array<(int,int)>, array<array<(int,int)>>) = (~ctx,~frmExpr,~expr) => {
    let constPartsToArr = (constParts:constParts) => {
        constParts.begins->Js_array2.mapi((b,i)=>(b,constParts.ends[i]))
    }
    let frmConstParts = createConstParts(frmExpr)
    let constParts = createMatchingConstParts(frmConstParts)
    let parenCnt = parenCntMake(~begin=ctx->makeExpr(["(", "[", "{"]), ~end=ctx->makeExpr([")", "]", "}"]))
    let matchingConstParts = []
    iterateConstParts(
        ~frmExpr, 
        ~expr, 
        ~frmConstParts, 
        ~constParts, 
        ~idxToMatch=0,
        ~parenCnt,
        ~consumer = constParts => {
            matchingConstParts->Js_array2.push(
                constPartsToArr(constParts)
            )->ignore
            Continue
        }
    )->ignore
    (
        constPartsToArr(frmConstParts),
        matchingConstParts
    )
}

let test_iterateSubstitutions: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr) => array<array<expr>> = (~ctx, ~frmExpr, ~expr) => {
    let frmConstParts = createConstParts(frmExpr)
    let constParts = createMatchingConstParts(frmConstParts)
    let parenCnt = parenCntMake(~begin=ctx->makeExpr(["(", "[", "{"]), ~end=ctx->makeExpr([")", "]", "}"]))
    let varGroups = createVarGroups(~frmExpr, ~frmConstParts)
    let numOfVars = frmExpr
        ->Js_array2.filter(i => i >= 0)
        ->Expln_utils_common.arrIntDistinct
        ->Js_array2.length
    let subs = createSubs(~numOfVars)
    let result = []
    iterateSubstitutions(
        ~frmExpr, 
        ~expr, 
        ~frmConstParts, 
        ~constParts, 
        ~varGroups,
        ~subs,
        ~parenCnt,
        ~consumer = subs => {
            let res = []
            for i in 0 to numOfVars-1 {
                res->Js_array2.push(
                    subs.exprs[i]->Js_array2.slice(~start=subs.begins[i], ~end_=subs.ends[i]+1)
                )->ignore
            }
            result->Js_array2.push(res)->ignore
            Continue
        }
    )
    result
}