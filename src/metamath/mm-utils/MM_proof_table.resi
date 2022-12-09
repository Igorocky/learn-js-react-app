open MM_context
open MM_parser
open MM_proof_verifier
open MM_substitution

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    label: string,
    expr:expr,
    mutable proof:option<exprSource>,
    mutable dist:int,
    mutable branches: option<array<exprSource>>
}

type proofTable = {
    existingIds: Belt_SetString.t,
    frms: Belt_MapString.t<frmSubsData>,
    preVarsNum: int,
}

let getNextExprToProveIdx: proofTable => option<int>
let addExprToProve: (proofTable, expr) => int
let markProved: proofTable => unit
let updateDist: (proofTable, int) => unit

let createProof: (mmContext, proofTable, int) => proof
let createOrderedProofTableFromProof: proofNode => proofTable

let proofTablePrint: (mmContext, proofTable, string) => unit