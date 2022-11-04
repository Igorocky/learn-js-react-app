open MM_parser

type expr = array<int>

type mmContext

type hypothesisType = F | E

type hypothesis = {
    typ: hypothesisType,
    label: string,
    expr: expr
}

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
let applySingleStmt: (mmContext, stmt) => unit
let openChildContext: mmContext => unit
let closeChildContext: mmContext => unit
let loadContext: (mmAstNode, ~initialContext:mmContext=?, ~stopBefore:string=?, ~stopAfter:string=?, ()) => mmContext

let isConst: (mmContext,string) => bool
let isVar: (mmContext,string) => bool
let getHypothesis: (mmContext,string) => option<hypothesis>
let getMandHyps: (mmContext, expr) => array<hypothesis>
let getFrame: (mmContext,string) => option<frame>
let makeExpr: (mmContext,array<string>) => expr
let ctxIntToStrExn: (mmContext, int) => string
let ctxExprToStrExn: (mmContext, expr) => string
let frmIntToStrExn: (mmContext, frame, int) => string
let frmExprToStrExn: (mmContext, frame, expr) => string
let forEachFrame: (mmContext, frame => option<'a>) => option<'a>
let forEachHypothesis: (mmContext, hypothesis => option<'a>) => option<'a>
let createFrame: (mmContext, string, array<string>) => frame
let getNestingLevel: mmContext => int
let findParentheses: mmContext => array<int>

let exprEq: (expr,expr) => bool