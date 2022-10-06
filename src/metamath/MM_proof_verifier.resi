open MM_parser

type mmContext

let createEmptyContext: unit => mmContext
let openChildContext: mmContext => mmContext
let closeChildContext: mmContext => result<mmContext,string>

//let addComment: (mmContext,string) => result<unit,string>
//let addConst: (mmContext,string) => result<unit,string>
//let addVar: (mmContext,string) => result<unit,string>
//let addDisj: (mmContext,array<string>) => result<unit,string>
//let addFloating: (mmContext, ~label:string, ~expr:array<string>) => result<unit,string>
//let addEssential: (mmContext, ~label:string, ~expr:array<string>) => result<unit,string>
//let addAxiom: (mmContext, ~label:string, ~expr:array<string>) => result<unit,string>
//let addProvable: (mmContext, ~label:string, ~expr:array<string>, ~proof:proof) => result<unit,string>

//let isConst: (mmContext,string) => bool
//let isVar: (mmContext,string) => bool