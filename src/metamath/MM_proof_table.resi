open MM_context
open MM_parser

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    mutable proved:bool,
    mutable dist:int,
    mutable src: option<array<exprSource>>
}

type proofTable = array<proofRecord>

let createSyntaxProofTable: expr => proofTable
let getNextExprToProveIdx: proofTable => option<int>
let addExprToProve: (proofTable, expr) => int
let markProved: proofTable => unit
let updateDist: proofTable => unit
let createProof: (mmContext, string, proofTable) => (frame,proof)