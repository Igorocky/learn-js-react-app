open MM_parser

type expr = array<int>

type hypothesis =
    | F(expr)
    | E(expr)

type frame = {
    disj: Belt.Map.Int.t<Belt_SetInt.t>,
    hyps: array<hypothesis>,
    asrt: expr,
    label: string,
    description: string,
    varNames: array<string>,
    varTypes: array<string>,
}

type rec mmContext = {
    parent: option<mmContext>,
    consts: array<string>,
    vars: array<string>,
    symToInt: Belt.MutableMap.String.t<int>,
    disj: Belt.MutableMap.Int.t<Belt_MutableSetInt.t>,
    hyps: array<hypothesis>,
    mutable lastComment: option<string>,
    frames: Belt.MutableMap.String.t<frame>,
}

let createEmptyContext: unit => mmContext = () => {
    parent: None,
    consts: [""],
    vars: [],
    symToInt: Belt.MutableMap.String.make(),
    disj: Belt.MutableMap.Int.make(),
    hyps: [],
    lastComment: None,
    frames: Belt.MutableMap.String.make(),
}

let openChildContext: mmContext => mmContext = ctx => {
    parent: Some(ctx),
    consts: ctx.consts->Js_array2.copy,
    vars: ctx.vars->Js_array2.copy,
    symToInt: ctx.symToInt->Belt.MutableMap.String.map(x=>x),
    disj: ctx.disj->Belt.MutableMap.Int.map(x=>x),
    hyps: ctx.hyps->Js_array2.copy,
    lastComment: ctx.lastComment,
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

let addComment: (mmContext,string) => result<unit,string> = (ctx,str) => {
    Ok(ctx.lastComment = Some(str))
}

let isConst: (mmContext,string) => bool = (ctx, sym) => 
    ctx.symToInt->Belt_MutableMapString.get(sym)->Belt_Option.map(i => i < 0)->Belt_Option.getWithDefault(false)

let isVar: (mmContext,string) => bool = (ctx, sym) => 
    ctx.symToInt->Belt_MutableMapString.get(sym)->Belt_Option.map(i => i >= 0)->Belt_Option.getWithDefault(false)

let addConst: (mmContext,string) => result<unit,string> = (ctx,cName) => {
    if (ctx.symToInt->Belt_MutableMapString.has(cName)) {
        Error(`An attempt to re-declare the math symbol '${cName}' as a constant.`)
    } else if (ctx.parent->Belt_Option.isSome) {
        Error(`An attempt to declare a constant '${cName}' in an inner block.`)
    } else {
        ctx.symToInt->Belt_MutableMapString.set(cName, -(ctx.consts->Js_array2.length))
        let _ = ctx.consts->Js_array2.push(cName)
        Ok(())
    }
}

let addVar: (mmContext,string) => result<unit,string> = (ctx,vName) => {
    if (ctx.symToInt->Belt_MutableMapString.has(vName)) {
        Error(`An attempt to re-declare the math symbol '${vName}' as a variable.`)
    } else {
        ctx.symToInt->Belt_MutableMapString.set(vName, ctx.vars->Js_array2.length)
        let _ = ctx.consts->Js_array2.push(vName)
        Ok(())
    }
}

let addDisjPair = (ctx, n, m) => {
    switch ctx.disj->Belt_MutableMapInt.get(n) {
        | None => ctx.disj->Belt_MutableMapInt.set(n, Belt_MutableSetInt.fromArray([m]))
        | Some(set) => set->Belt_MutableSetInt.add(m)
    }
}

let addDisj: (mmContext,array<string>) => result<unit,string> = (ctx, vars) => {
    switch vars->Js_array2.find(sym => !(ctx->isVar(sym))) {
        | Some(sym) => Error(`The symbol '${sym}' is not a variable but it ocurs inside of a disjoint statement.`)
        | None => {
            let varInts = vars->Js_array2.map(ctx.symToInt->Belt_MutableMapString.getExn)
            let maxIdx = varInts->Js_array2.length - 1
            for i in 0 to maxIdx {
                for j in i+1 to maxIdx {
                    let n = varInts[i]
                    let m = varInts[j]
                    ctx->addDisjPair(n,m)
                    ctx->addDisjPair(m,n)
                }
            }
            Ok(())
        }
    }
}

//let addFloating: (mmContext, ~label:string, ~expr:array<string>) => result<unit,string>

//let addEssential: (mmContext, ~label:string, ~expr:array<string>) => result<unit,string>

//let addAxiom: (mmContext, ~label:string, ~expr:array<string>) => result<unit,string>

//let addProvable: (mmContext, ~label:string, ~expr:array<string>, ~proof:proof) => result<unit,string>

