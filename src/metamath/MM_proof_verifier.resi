open MM_parser

type mmContext

let createEmptyContext: unit => mmContext
let createContext: stmt => result<mmContext,string>
let openChildContext: mmContext => mmContext
let closeChildContext: mmContext => result<mmContext,string>
let applyStmt: (mmContext, stmt) => result<unit,string>
