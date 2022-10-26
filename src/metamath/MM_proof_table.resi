open MM_context
open MM_parser
open MM_proof_verifier

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

let createProofTable: expr => proofTable
let createProofTableFromProof: (mmContext, proofNode) => proofTable 
let createOrderedProofTableFromProof: (mmContext, proofNode) => proofTable 
let getNextExprToProveIdx: proofTable => option<int>
let addExprToProve: (proofTable, expr) => int
let markProved: proofTable => unit
let updateDist: proofTable => unit
let createProof: (mmContext, proofTable) => proof
let proofTablePrint: (mmContext, proofTable, string) => unit