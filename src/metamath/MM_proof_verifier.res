open MM_parser

type expr = array<int>

type mandHyp =
    | F(expr)
    | E(expr)

type frame = {
    disj: Belt.Map.Int.t<int>,
    hyps: array<mandHyp>,
    asrt: expr,
    label: string,
    description: string,
    varNames: array<string>,
}

type rec mmContext = {
    parent: option<mmContext>,
    consts: array<string>,
    vars: array<string>,
    symToInt: Belt.MutableMap.String.t<int>,
    disj: Belt.MutableMap.Int.t<int>,
    frames: Belt.MutableMap.String.t<frame>,
}

let createEmptyContext: unit => mmContext = () => {
    parent: None,
    consts: [""],
    vars: [],
    symToInt: Belt.MutableMap.String.make(),
    disj: Belt.MutableMap.Int.make(),
    frames: Belt.MutableMap.String.make(),
}

let openChildContext: mmContext => mmContext = ctx => {
    parent: Some(ctx),
    consts: ctx.consts->Js_array2.copy,
    vars: ctx.vars->Js_array2.copy,
    symToInt: ctx.symToInt->Belt.MutableMap.String.map(x=>x),
    disj: ctx.disj->Belt.MutableMap.Int.map(x=>x),
    frames: ctx.frames->Belt.MutableMap.String.map(x=>x),
}

let closeChildContext: mmContext => result<mmContext,string> = ctx => {
    switch ctx.parent {
        | None => Error(`Cannot close the root context.`)
        | Some(parent) => Ok({
            ...parent,
            frames: ctx.frames
        })
    }
}

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