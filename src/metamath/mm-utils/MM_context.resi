open MM_parser

type expr = array<int>

type mmContext

type hypothesisType = F | E

type hypothesis = {
    typ: hypothesisType,
    label: string,
    expr: expr
}

type disjMutable

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

let createContext: (~parent:mmContext=?, ()) => mmContext
let applySingleStmt: (mmContext, stmt) => unit
let addLocalConst: (mmContext, string) => unit
let openChildContext: mmContext => unit
let closeChildContext: mmContext => unit
let resetToParentContext: mmContext => unit
let cloneContext: mmContext => mmContext
let loadContext: (mmAstNode, ~initialContext:mmContext=?, ~stopBefore:string=?, ~stopAfter:string=?, 
                    ~expectedNumOfAssertions:int=?, ~onProgress:float=>unit=?, ()) => mmContext

let isConst: (mmContext,string) => bool
let isVar: (mmContext,string) => bool
let isHyp: (mmContext,string) => bool
let isAsrt: (mmContext,string) => bool
let isDisj: (mmContext,int,int) => bool
let getHypothesis: (mmContext,string) => option<hypothesis>
let getMandHyps: (mmContext, expr) => array<hypothesis>
let getAllHyps: mmContext => Belt_MapString.t<hypothesis>
let getAllDisj: mmContext => disjMutable
let getFrame: (mmContext,string) => option<frame>
let getLocalVars: mmContext => array<string>
let getLocalConsts: mmContext => array<string>
let getLocalHyps: mmContext => array<hypothesis>
let getNumOfVars: mmContext => int
let ctxSymbToInt: (mmContext, string) => option<int>
let ctxSymbToIntExn: (mmContext, string) => int
let makeExprExn: (mmContext,array<string>) => expr
let makeExprFromStringExn: (mmContext, string) => expr
let ctxIntToStrExn: (mmContext, int) => string
let ctxIntToStr: (mmContext, int) => option<string>
let ctxExprToStrExn: (mmContext, expr) => string
let frmIntToStrExn: (mmContext, frame, int) => string
let frmExprToStrExn: (mmContext, frame, expr) => string
let forEachFrame: (mmContext, frame => option<'a>) => option<'a>
let forEachHypothesisInDeclarationOrder: (mmContext, hypothesis => option<'a>) => option<'a>
let createFrame: (mmContext, string, array<string>) => frame
let getNestingLevel: mmContext => int
let findParentheses: (mmContext, ~onProgress:float=>unit=?, unit) => array<int>
let generateNewVarNames: (mmContext, array<int>) => array<string>
let generateNewLabels: (mmContext, ~prefix:string, ~amount:int) => array<string>

let exprEq: (expr,expr) => bool
let disjMutableMake: unit => disjMutable
let addDisjPairToMap: (disjMutable, int, int) => unit
let disjContains: (disjMutable, int, int) => bool
let disjForEachArr: (disjMutable, array<int> => unit) => unit
let disjForEach: (disjMutable, (int,int) => unit) => unit

module ExprCmp: {
  type identity
  type t = expr
  let cmp: Belt_Id.cmp<t, identity>
}