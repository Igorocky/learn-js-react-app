open MM_parser

type expr = array<int>

type mmContext

type hypothesis =
    | F(expr)
    | E(expr)

type frame = {
    disj: Belt.Map.Int.t<Belt_SetInt.t>,
    hyps: array<hypothesis>,
    asrt: expr,
    label: string,
    description: string,
    intToSymb: Belt_MapInt.t<string>,
    varTypes: array<int>,
    numOfVars: int,
    numOfArgs: int,
}

let createEmptyContext: unit => mmContext
let createContext: mmAstNode => mmContext
let openChildContext: mmContext => unit
let closeChildContext: mmContext => unit
let applyStmt: (mmContext, mmAstNode) => unit

let getHypothesisExpr: (mmContext,string) => option<expr>
let getFrame: (mmContext,string) => option<frame>