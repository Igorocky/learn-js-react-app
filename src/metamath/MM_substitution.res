open Expln_utils_common
open MM_parenCounter
open MM_context

type contunieInstruction = Continue | Stop

type constParts = {
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

//let iterateSubs: (~expr:expr, ~frmExpr:expr, ~frame:frame, ~consumer:subs=>contunieInstruction) => unit