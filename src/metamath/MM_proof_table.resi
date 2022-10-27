open MM_context
open MM_parser
open MM_proof_verifier

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    mutable proof:option<exprSource>,
    mutable dist:int,
    mutable branches: option<array<exprSource>>
}

type proofTable = array<proofRecord>

let createProofTable: expr => proofTable
let createOrderedProofTableFromProof: proofNode => proofTable
let getNextExprToProveIdx: proofTable => option<int>
let addExprToProve: (proofTable, expr) => int
let markProved: proofTable => unit
let updateDist: proofTable => unit
let createProof: (mmContext, proofTable) => proof
let proofTablePrint: (mmContext, proofTable, string) => unit