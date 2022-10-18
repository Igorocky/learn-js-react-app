open Expln_utils_common
open MM_parenCounter
open MM_context

type contunieInstruction = Continue | Stop

type constParts = {
    length: int,
    begins: array<int>,
    ends: array<int>,
    parenCnts:array<parenCnt>,
    remainingMinLength: array<int>,
}

type subs

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
        if (constPartsLength == 0 || constParts[constPartsLength-1][1] >= 0) {
            constParts->Js_array2.push([i,-1])->ignore
        } else if (constPartsLength > 0 && constParts[constPartsLength-1][1] < 0) {
            constParts[constPartsLength-1][1] = i-1
        }
    }
    let constPartsLength = constParts->Js_array2.length
    if (constPartsLength > 0 && constParts[constPartsLength-1][1] < 0) {
        constParts[constPartsLength-1][1] = constPartsLength-1
    }
    let result = {
        length: constPartsLength,
        begins: createArray(constPartsLength),
        ends: createArray(constPartsLength),
        parenCnts: [],
        remainingMinLength: createArray(constPartsLength)
    }
    let exprLength = expr->Js_array2.length
    let remainingMinLength = ref(0)
    for i in constPartsLength-1 downto 0 {
        result.begins[i] = constParts[i][0]
        result.ends[i] = constParts[i][1]
        remainingMinLength.contents = remainingMinLength.contents + (result.ends[i] - result.begins[i] + 1) + lengthOfGap(i, constParts, exprLength)
        result.remainingMinLength[i] = remainingMinLength.contents
    }
    result
}

let createMatchingConstParts = (~constParts:constParts, ~parenCntFactory:()=>parenCnt) => {
    {
        length: constParts.length,
        begins: createArray(constParts.length),
        ends: createArray(constParts.length),
        //todo: use single parenCounter
        parenCnts: Belt_Array.range(0,constParts.length)->Js.Array2.map(_=>parenCntFactory()), //parenCnts[i] is for the gap before ith const part
        remainingMinLength: []
    }
}

let rec iterateConstParts = (
    ~frmExpr:expr, 
    ~expr:expr, 
    ~frmConstParts:constParts, 
    ~constParts:constParts, 
    ~idxToMatch:int, 
    ~consumer:(~frmConstParts:constParts,~constParts:constParts) => contunieInstruction
):contunieInstruction => {
    let invokeNext = ():contunieInstruction => {
        iterateConstParts(
            ~frmExpr, 
            ~expr, 
            ~frmConstParts, 
            ~constParts, 
            ~idxToMatch=idxToMatch+1, 
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
                    let pCnt = constParts.parenCnts[idxToMatch]
                    pCnt->parenCntReset
                    let pState = ref(Balanced)
                    let i = ref(constParts.ends[idxToMatch-1]+1)
                    while (i.contents < exprLen && pState.contents != Failed) {
                        pState.contents = pCnt->parenCntPut(expr[i.contents])
                        i.contents = i.contents + 1
                    }
                    if (pState.contents == Balanced) {
                        consumer(~frmConstParts,~constParts)
                    } else {
                        Continue
                    }
                }
            }
        } else {
            consumer(~frmConstParts,~constParts)
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
        let pCnt = constParts.parenCnts[idxToMatch]
        pCnt->parenCntReset
        let pState = ref(Balanced)
        let numOfVars = lengthOfGap2(idxToMatch-1,frmConstParts,frmExprLen)
        for i in 1 to numOfVars {
            pState.contents = pCnt->parenCntPut(expr[begin.contents])
            begin.contents = begin.contents + 1
        }
        let partLen = frmConstParts.ends[idxToMatch] - frmConstParts.begins[idxToMatch] + 1
        let instr = ref(Continue)
        while (begin.contents <= maxBegin && pState.contents != Failed && instr.contents == Continue) {
            if (pState.contents == Balanced) {
                let matchedLen = ref(0)
                let cmpRes = ref(true)
                while (matchedLen.contents < partLen && cmpRes.contents) {
                    cmpRes.contents = frmExpr[frmConstParts.begins[idxToMatch]+matchedLen.contents] != expr[begin.contents+matchedLen.contents]
                    matchedLen.contents = matchedLen.contents + 1
                }
                if (matchedLen.contents == partLen && cmpRes.contents) {
                    constParts.begins[idxToMatch] = begin.contents
                    constParts.ends[idxToMatch] = begin.contents+partLen-1
                    instr.contents = invokeNext()
                }
            }
            pState.contents = pCnt->parenCntPut(expr[begin.contents])
            begin.contents = begin.contents + 1
        }
        instr.contents
    }
}

//let iterateSubs: (~expr:expr, ~frmExpr:expr, ~frame:frame, ~consumer:subs=>contunieInstruction) => unit