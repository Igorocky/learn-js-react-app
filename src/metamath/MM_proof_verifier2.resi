open MM_parser

type mmContext

let createEmptyContext: unit => mmContext
let createContext: mmAstNode => mmContext
let openChildContext: mmContext => mmContext
let closeChildContext: mmContext => mmContext
let applyStmt: (mmContext, mmAstNode) => unit
