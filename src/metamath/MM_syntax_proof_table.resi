open MM_context
open MM_substitution
open MM_parser

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    proved:bool,
    dist:int,
    src: option<array<exprSource>>
}

type proofTable = array<proofRecord>

let createSyntaxProofTable: expr => proofTable
let getExprToProve: proofTable => option<int>