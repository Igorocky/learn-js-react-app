
type state = Balanced | Opened | Failed
type parenCnt

let parenCntMake: (~begin:array<int>, ~end:array<int>) => parenCnt
let parenCntReset: parenCnt => unit
let parenCntPut: (parenCnt,int) => state