open MM_context
open MM_parser
open MM_proof_verifier

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    proof:exprSource,
}

type proofTable2 = array<proofRecord>

let createProof: (mmContext, proofTable2, int) => proof
let createProofTableFromProof: proofNode => proofTable2

let proofTablePrint: (mmContext, proofTable2, string) => unit