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
    intToSymb: Belt_MapInt.t<string>,
    varTypes: array<int>,
}

type rec mmContext = {
    parent: option<mmContext>,
    consts: array<string>,
    vars: array<string>,
    symToInt: Belt.MutableMap.String.t<int>,
    disj: Belt.MutableMap.Int.t<Belt_MutableSetInt.t>,
    hyps: array<hypothesis>,
    symToHyp: Belt.MutableMap.String.t<expr>,
    mutable lastComment: string,
    frames: Belt.MutableMap.String.t<frame>,
}

let createEmptyContext: unit => mmContext = () => {
    parent: None,
    consts: [""],
    vars: [],
    symToInt: Belt.MutableMap.String.make(),
    disj: Belt.MutableMap.Int.make(),
    hyps: [],
    symToHyp: Belt.MutableMap.String.make(),
    lastComment: "",
    frames: Belt.MutableMap.String.make(),
}

let openChildContext: mmContext => mmContext = ctx => {
    parent: Some(ctx),
    consts: ctx.consts->Js_array2.copy,
    vars: ctx.vars->Js_array2.copy,
    symToInt: ctx.symToInt->Belt.MutableMap.String.map(x=>x),
    disj: ctx.disj->Belt.MutableMap.Int.map(x=>x),
    hyps: ctx.hyps->Js_array2.copy,
    symToHyp: ctx.symToHyp->Belt.MutableMap.String.map(x=>x),
    lastComment: ctx.lastComment,
    frames: ctx.frames->Belt.MutableMap.String.map(x=>x),
}

let closeChildContext: mmContext => mmContext = ctx => {
    switch ctx.parent {
        | None => raise(MmException({msg:`Cannot close the root context.`}))
        | Some(parent) => {
            ...parent,
            frames: ctx.frames
        }
    }
}

let addComment: (mmContext,string) => unit = (ctx,str) => {
    ctx.lastComment = str
}

let isConst: (mmContext,string) => bool = (ctx, sym) => 
    ctx.symToInt->Belt_MutableMapString.get(sym)->Belt_Option.map(i => i < 0)->Belt_Option.getWithDefault(false)

let isVar: (mmContext,string) => bool = (ctx, sym) => 
    ctx.symToInt->Belt_MutableMapString.get(sym)->Belt_Option.map(i => i >= 0)->Belt_Option.getWithDefault(false)

let addConst: (mmContext,string) => unit = (ctx,cName) => {
    if (ctx.symToInt->Belt_MutableMapString.has(cName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${cName}' as a constant.`}))
    } else if (ctx.parent->Belt_Option.isSome) {
        raise(MmException({msg:`An attempt to declare a constant '${cName}' in an inner block.`}))
    } else {
        ctx.symToInt->Belt_MutableMapString.set(cName, -(ctx.consts->Js_array2.length))
        let _ = ctx.consts->Js_array2.push(cName)
    }
}

let addVar: (mmContext,string) => unit = (ctx,vName) => {
    if (ctx.symToInt->Belt_MutableMapString.has(vName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${vName}' as a variable.`}))
    } else {
        ctx.symToInt->Belt_MutableMapString.set(vName, ctx.vars->Js_array2.length)
        let _ = ctx.vars->Js_array2.push(vName)
    }
}

let addDisjPairToMap = (disjMap, n, m) => {
    switch disjMap->Belt_MutableMapInt.get(n) {
        | None => disjMap->Belt_MutableMapInt.set(n, Belt_MutableSetInt.fromArray([m]))
        | Some(set) => set->Belt_MutableSetInt.add(m)
    }
}

let addDisjPair = (ctx, n, m) => addDisjPairToMap(ctx.disj,n,m)

let addDisj: (mmContext,array<string>) => unit = (ctx, vars) => {
    switch vars->Js_array2.find(sym => !(ctx->isVar(sym))) {
        | Some(sym) => raise(MmException({msg:`The symbol '${sym}' is not a variable but it is used in a disjoint statement.`}))
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
        }
    }
}

let addFloating: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    if (exprStr->Js_array2.length != 2) {
        raise(MmException({msg:`Length of a floating expression must be 2.`}))
    } else if (!(ctx->isConst(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in a floating expression must be a constant.`}))
    } else if (!(ctx->isVar(exprStr[1]))) {
        raise(MmException({msg:`The second symbol in a floating expression must be a variable.`}))
    } else if (ctx.symToHyp->Belt_MutableMapString.has(label)) {
        raise(MmException({msg:`Cannot reuse the label '${label}'`}))
    } else {
        let varName = exprStr[1]
        let varInt = ctx.symToInt->Belt_MutableMapString.getExn(varName)
        let existingFloating = ctx.hyps
            ->Js_array2.filter(hyp => {
                switch hyp {
                    | F([_,v]) => v == varInt
                    | _ => false
                }
            })
        if (existingFloating->Js_array2.length != 0) {
            raise(MmException({msg:`Cannot redefined typecode for the variable '${varName}'`}))
        } else {
            let expr = exprStr->Js_array2.map(ctx.symToInt->Belt_MutableMapString.getExn)
            let _ = ctx.hyps->Js_array2.push(F(expr))
            ctx.symToHyp->Belt_MutableMapString.set(label, expr)
        }
    }
}

let addEssential: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an essential expression must be at least 1.`}))
    } else if (!(ctx->isConst(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an essential expression must be a constant.`}))
    } else {
        switch exprStr->Js_array2.find(sym => !(ctx.symToInt->Belt_MutableMapString.has(sym))) {
            | Some(sym) => raise(MmException({msg:`The symbol '${sym}' must be either a constant or a variable.`}))
            | None => {
                let expr = exprStr->Js_array2.map(ctx.symToInt->Belt_MutableMapString.getExn)
                let _ = ctx.hyps->Js_array2.push(E(expr))
                ctx.symToHyp->Belt_MutableMapString.set(label, expr)
            }
        }
    }
}

let extractMandatoryVariables = (ctx,asrt) => {
    let extractVarsFromExpr = Js_array2.filter(_, i => i >= 0)
    Belt_SetInt.fromArray(
        Js_array2.concatMany(
            extractVarsFromExpr(asrt),
            ctx.hyps
                ->Js_array2.filter(hyp => {
                    switch hyp {
                        | E(expr) => true
                        | _ => false
                    }
                })
                ->Js_array2.map(hyp => {
                    switch hyp {
                        | E(expr) => extractVarsFromExpr(expr)
                        | _ => []
                    }
                })
        )
    )
}

let extractMandatoryDisj = (ctx, mandatoryVars): Belt_MapInt.t<Belt_SetInt.t> => {
    let mandatoryDisj = Belt_MutableMapInt.make()
    ctx.disj->Belt_MutableMapInt.forEach((n,ms) => {
        if (mandatoryVars->Belt_SetInt.has(n)) {
            ms->Belt_MutableSetInt.forEach(m => {
                if (mandatoryVars->Belt_SetInt.has(m)) {
                    addDisjPairToMap(mandatoryDisj, n, m)
                }
            })
        }
    })
    mandatoryDisj
        ->Belt_MutableMapInt.toArray
        ->Js_array2.map(((i,set)) => (i, set->Belt_MutableSetInt.toArray->Belt_SetInt.fromArray))
        ->Belt_MapInt.fromArray
}

let extractMandatoryHypotheses = (ctx, mandatoryVars) => {
    ctx.hyps
        ->Js_array2.filter(hyp => {
            switch hyp {
                | F([_,v]) => mandatoryVars->Belt_SetInt.has(v)
                | E(_) => true
            }
        })
}

let renumberVarsInDisj = (disj: Belt_MapInt.t<Belt_SetInt.t>, renumbering: Belt_MapInt.t<int>): Belt_MapInt.t<Belt_SetInt.t> => {
    disj
        ->Belt_MapInt.toArray
        ->Js_array2.map( ((n,ms)) => {
            (
                renumbering->Belt_MapInt.getExn(n), 
                ms->Belt_SetInt.toArray->Js_array2.map(renumbering->Belt_MapInt.getExn)->Belt_SetInt.fromArray
            )
        })
        ->Belt_MapInt.fromArray
}

let renumberVarsInExpr = (expr: expr, renumbering: Belt_MapInt.t<int>): expr => {
    expr->Js_array2.map(i => if (i<0) {i} else {renumbering->Belt_MapInt.getExn(i)})
}

let renumberVarsInHypothesis = (hyp: hypothesis, renumbering: Belt_MapInt.t<int>): hypothesis => {
    switch hyp {
        | F(expr) => F(renumberVarsInExpr(expr, renumbering))
        | E(expr) => E(renumberVarsInExpr(expr, renumbering))
    }
}

let createIntToSymbMap = (ctx, mandatoryHypotheses:array<hypothesis>, asrt, varRenumbering: Belt_MapInt.t<int>): Belt_MapInt.t<string> => {
    asrt->Js_array2.concatMany(
        mandatoryHypotheses->Js_array2.map(hyp => {
            switch hyp {
                | F(expr) => expr
                | E(expr) => expr
            }
        })
    )
        ->Belt_SetInt.fromArray
        ->Belt_SetInt.toArray
        ->Js_array2.map(i => {
            if (i < 0) {
                (i, ctx.consts[-i])
            } else {
                (varRenumbering->Belt_MapInt.getExn(i), ctx.vars[i])
            }
        })
        ->Belt_MapInt.fromArray
}

let extractVarTypes = (ctx, mandatoryVars: Belt_SetInt.t, varRenumbering: Belt_MapInt.t<int>): array<int> => {
    let varTypes = Expln_utils_common.createArray(mandatoryVars->Belt_SetInt.size)
    ctx.hyps->Js_array2.forEach(hyp => {
        switch hyp {
            | F([t,v]) => {
                if (mandatoryVars->Belt_SetInt.has(v)) {
                    varTypes[varRenumbering->Belt_MapInt.getExn(v)] = t
                }
            }
            | _ => ()
        }
    })
    varTypes
}

let createFrame: (mmContext, ~label:string, ~exprStr:array<string>) => frame = (ctx, ~label, ~exprStr) => {
    if (ctx.frames->Belt_MutableMapString.has(label)) {
        raise(MmException({msg:`The label '${label}' is already used for another assertion.`}))
    } else if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an assertion expression must be at least 1.`}))
    } else if (!(ctx->isConst(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an assertion expression must be a constant.`}))
    } else {
        switch exprStr->Js_array2.find(sym => !(ctx.symToInt->Belt_MutableMapString.has(sym))) {
            | Some(sym) => raise(MmException({msg:`The symbol '${sym}' must be either a constant or a variable.`}))
            | None => {
                let asrt: expr = exprStr->Js_array2.map(ctx.symToInt->Belt_MutableMapString.getExn)
                let mandatoryVars: Belt_SetInt.t = extractMandatoryVariables(ctx, asrt)
                let mandatoryDisj: Belt_MapInt.t<Belt_SetInt.t> = extractMandatoryDisj(ctx, mandatoryVars)
                let mandatoryHypotheses: array<hypothesis> = extractMandatoryHypotheses(ctx, mandatoryVars)
                let varRenumbering: Belt_MapInt.t<int> = mandatoryVars -> Belt_SetInt.toArray -> Js_array2.mapi((v,i) => (v,i)) -> Belt_MapInt.fromArray
                {
                    disj: mandatoryDisj->renumberVarsInDisj(varRenumbering),
                    hyps: mandatoryHypotheses->Js_array2.map(renumberVarsInHypothesis(_, varRenumbering)),
                    asrt: asrt->renumberVarsInExpr(varRenumbering),
                    label: label,
                    description: ctx.lastComment,
                    intToSymb: createIntToSymbMap(ctx, mandatoryHypotheses, asrt, varRenumbering),
                    varTypes: extractVarTypes(ctx, mandatoryVars, varRenumbering)
                }
            }
        }
    }
}

let addAssertion: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    ctx.frames->Belt_MutableMapString.set(label, createFrame(ctx, ~label, ~exprStr))
}

let strJoin = (ss:array<string>, ~sep:option<string>=?):string => {
    switch sep {
        | None => ""->Js.String2.concatMany(ss)
        | Some(sep) => {
            let lastIdx = ss->Js.Array2.length - 1
            ss->Js.Array2.mapi((s,i) => if (i != lastIdx) {s++sep} else {s})->Js_string2.concatMany("", _)
        }
    }
}

let printListOfSymbols = ss => strJoin(ss, ~sep=" ")

let stmtToString = ast => {
    switch ast {
        | {stmt:Comment({text})} => `Comment()`
        | {stmt:Const({symbols})} => `Const({symbols:${printListOfSymbols(symbols)}})`
        | {stmt:Block({statements})} => `Block({statements:${"array of " ++ Expln_utils_common.i2s(statements->Js_array2.length) ++ " items"}})`
        | {stmt:Var({symbols})} => `Var({symbols:${printListOfSymbols(symbols)}})`
        | {stmt:Disj({vars})} => `Disj({vars:${printListOfSymbols(vars)}})`
        | {stmt:Floating({label, expr})} => `Floating({label:'${label}', expr:symbols:${printListOfSymbols(expr)}})`
        | {stmt:Essential({label, expr})} => `Essential({label:'${label}', expr:${printListOfSymbols(expr)}})`
        | {stmt:Axiom({label, expr})} => `Axiom({label:'${label}', expr:${printListOfSymbols(expr)}})`
        | {stmt:Provable({label, expr, proof})} => `Provable({label:'${label}', expr:${printListOfSymbols(expr)}})`
    }
}

let rec applyStmt = (ctx:mmContext, stmt:mmAstNode):unit => {
    Js.log("applyStmt: " ++ stmtToString(stmt))
    try {
        switch stmt {
            | {stmt:Comment({text})} => addComment(ctx, text)
            | {stmt:Const({symbols})} => symbols->Js_array2.forEach(addConst(ctx, _))
            | {stmt:Block({statements})} => {
                //openChildContext(ctx)
                statements->Js_array2.forEach(applyStmt(ctx, _))
            }
            | {stmt:Var({symbols})} => symbols->Js_array2.forEach(addVar(ctx, _))
            | {stmt:Disj({vars})} => addDisj(ctx, vars)
            | {stmt:Floating({label, expr})} => addFloating(ctx, ~label, ~exprStr=expr)
            | {stmt:Essential({label, expr})} => addEssential(ctx, ~label, ~exprStr=expr)
            | {stmt:Axiom({label, expr})} => addAssertion(ctx, ~label, ~exprStr=expr)
            | {stmt:Provable({label, expr})} => addAssertion(ctx, ~label, ~exprStr=expr)
        }
    } catch {
        | MmException(ex) => raise(MmException({msg:ex.msg, begin:stmt.begin}))
    }

}

let createContext: mmAstNode => mmContext = stmt => {
    let ctx = createEmptyContext()
    applyStmt(ctx, stmt)
    ctx
}
