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
    frameVarToSymb: Belt_MapInt.t<string>,
    varTypes: array<int>,
    numOfVars: int,
    numOfArgs: int,
}

let createEmptyContext: unit => mmContext
let createContext: (mmAstNode, ~stopBefore:string=?, ~stopAfter:string=?, ()) => mmContext
let openChildContext: mmContext => unit
let closeChildContext: mmContext => unit
let applyStmt: (mmContext, mmAstNode, ~stopBefore:string=?, ~stopAfter:string=?, ()) => bool

let getHypothesisExpr: (mmContext,string) => option<expr>
let getFrame: (mmContext,string) => option<frame>
let makeExpr: (mmContext,array<string>) => expr
let ctxExprToStr: (mmContext, expr) => array<string>
let frameExprToStr: (mmContext, frame, expr) => array<string>
let getMandHyps: (mmContext, expr) => array<hypothesis>
let hypToExpr: hypothesis => expr
