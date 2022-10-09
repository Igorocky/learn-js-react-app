open MM_parser

type mmContext

let createEmptyContext: unit => mmContext
let createContext: mmAstNode => mmContext
let openChildContext: mmContext => unit
let closeChildContext: mmContext => unit
let applyStmt: (mmContext, mmAstNode) => unit

