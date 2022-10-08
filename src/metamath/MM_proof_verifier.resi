open MM_parser

type mmContext

let createEmptyContext: unit => mmContext
let createContext: mmAstNode => result<mmContext,(string,int)>
let openChildContext: mmContext => mmContext
let closeChildContext: mmContext => result<mmContext,string>
let applyStmt: (mmContext, mmAstNode) => result<unit,(string,int)>
