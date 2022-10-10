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
    frameVarToSymb: Belt_MapInt.t<string>,
    varTypes: array<int>,
    numOfVars: int,
    numOfArgs: int,
}

type rec mmContext = {
    mutable parent: option<mmContext>,
    mutable consts: array<string>,
    mutable vars: array<string>,
    mutable symToInt: Belt.MutableMap.String.t<int>,
    mutable disj: Belt.MutableMap.Int.t<Belt_MutableSetInt.t>,
    mutable hyps: array<hypothesis>,
    mutable symToHyp: Belt.MutableMap.String.t<expr>,
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

let cloneMutableMapString = (mmi:Belt_MutableMapString.t<'v>, cloneValue:'v=>'v) => mmi->Belt_MutableMapString.map(cloneValue)
let cloneMutableMapInt = (mmi:Belt_MutableMapInt.t<'v>, cloneValue:'v=>'v) => mmi->Belt_MutableMapInt.map(cloneValue)
let cloneMutableSetInt = (ms:Belt_MutableSetInt.t) => ms->Belt_MutableSetInt.copy

let cloneContext: mmContext => mmContext = ctx => {
    parent: ctx.parent,
    consts: ctx.consts->Js_array2.copy,
    vars: ctx.vars->Js_array2.copy,
    symToInt: ctx.symToInt->cloneMutableMapString(x=>x),
    disj: ctx.disj->cloneMutableMapInt(cloneMutableSetInt),
    hyps: ctx.hyps->Js_array2.copy,
    symToHyp: ctx.symToHyp->cloneMutableMapString(x=>x),
    lastComment: ctx.lastComment,
    frames: ctx.frames->cloneMutableMapString(x=>x),
}

let openChildContext: mmContext => unit = ctx => {
    ctx.parent = Some(ctx->cloneContext)
}

let closeChildContext: mmContext => unit = ctx => {
    switch ctx.parent {
        | None => raise(MmException({msg:`Cannot close the root context.`}))
        | Some(parent) => {
            ctx.parent = parent.parent
            ctx.consts = parent.consts
            ctx.vars = parent.vars
            ctx.symToInt = parent.symToInt
            ctx.disj = parent.disj
            ctx.hyps = parent.hyps
            ctx.symToHyp = parent.symToHyp
            ctx.lastComment = parent.lastComment
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
        ctx.consts->Js_array2.push(cName)->ignore
    }
}

let addVar: (mmContext,string) => unit = (ctx,vName) => {
    if (ctx.symToInt->Belt_MutableMapString.has(vName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${vName}' as a variable.`}))
    } else {
        ctx.symToInt->Belt_MutableMapString.set(vName, ctx.vars->Js_array2.length)
        ctx.vars->Js_array2.push(vName)->ignore
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
            ctx.hyps->Js_array2.push(F(expr))->ignore
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
                ctx.hyps->Js_array2.push(E(expr))->ignore
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
                        | E(_) => true
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
                | _ => true
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

let hypToExpr: hypothesis => expr = hyp => {
    switch hyp {
        | F(expr) | E(expr) => expr
    }
}

let createFrameVarToSymbMap = (ctx, mandatoryHypotheses:array<hypothesis>, asrt, varRenumbering: Belt_MapInt.t<int>): Belt_MapInt.t<string> => {
    asrt->Js_array2.concatMany(
        mandatoryHypotheses->Js_array2.map(hypToExpr)
    )
        ->Js_array2.filter(i => i >= 0)
        ->Belt_SetInt.fromArray
        ->Belt_SetInt.toArray
        ->Js_array2.map(i => {
            (varRenumbering->Belt_MapInt.getExn(i), ctx.vars[i])
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
                let varTypes = extractVarTypes(ctx, mandatoryVars, varRenumbering)
                let hyps = mandatoryHypotheses->Js_array2.map(renumberVarsInHypothesis(_, varRenumbering))
                {
                    disj: mandatoryDisj->renumberVarsInDisj(varRenumbering),
                    hyps,
                    asrt: asrt->renumberVarsInExpr(varRenumbering),
                    label,
                    description: ctx.lastComment,
                    frameVarToSymb: createFrameVarToSymbMap(ctx, mandatoryHypotheses, asrt, varRenumbering),
                    varTypes,
                    numOfVars: varTypes->Js_array2.length,
                    numOfArgs: hyps->Js_array2.length
                }
            }
        }
    }
}

let addAssertion: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    ctx.frames->Belt_MutableMapString.set(label, createFrame(ctx, ~label, ~exprStr))
}

let rec applyStmt = (ctx:mmContext, ast:mmAstNode, ~stopBefore:option<string>=?, ~stopAfter:option<string>=?, ()):bool => {
    let shouldStop = (expectedAsrtLabel, currentAsrtLabel) => {
        switch expectedAsrtLabel {
            | Some(expectedAsrtLabel) => expectedAsrtLabel == currentAsrtLabel
            | None => false
        }
    }
    try {
        let continue = ref(true)
        switch ast {
            | {stmt:Comment({text})} => addComment(ctx, text)
            | {stmt:Const({symbols})} => symbols->Js_array2.forEach(addConst(ctx, _))
            | {stmt:Block({level,statements})} => {
                if (level > 0) {
                    openChildContext(ctx)
                }
                let i = ref(0)
                let len = statements->Js_array2.length
                while (i.contents < len && continue.contents) {
                    continue.contents = applyStmt(ctx, statements[i.contents], ~stopBefore=?stopBefore, ~stopAfter=?stopAfter, ())
                    i.contents = i.contents + 1
                }
                if (continue.contents && level > 0) {
                    closeChildContext(ctx)
                }
            }
            | {stmt:Var({symbols})} => symbols->Js_array2.forEach(addVar(ctx, _))
            | {stmt:Disj({vars})} => addDisj(ctx, vars)
            | {stmt:Floating({label, expr})} => addFloating(ctx, ~label, ~exprStr=expr)
            | {stmt:Essential({label, expr})} => addEssential(ctx, ~label, ~exprStr=expr)
            | {stmt:Axiom({label, expr})} | {stmt:Provable({label, expr})} => {
                if (shouldStop(stopBefore, label)) {
                    continue.contents = false
                } else {
                    addAssertion(ctx, ~label, ~exprStr=expr)
                    continue.contents = !shouldStop(stopAfter, label)
                }
            }
        }
        continue.contents
    } catch {
        | MmException(ex) => raise(MmException({msg:ex.msg, begin:ex.begin->Belt.Option.getWithDefault(ast.begin)}))
    }
}

let createContext: (mmAstNode, ~stopBefore:string=?, ~stopAfter:string=?, ()) => mmContext = (ast, ~stopBefore:option<string>=?, ~stopAfter:option<string>=?, ()) => {
    let ctx = createEmptyContext()
    applyStmt(ctx, ast, ~stopBefore=?stopBefore, ~stopAfter=?stopAfter, ())->ignore
    ctx
}

let getHypothesisExpr: (mmContext,string) => option<expr> = (ctx,label) => ctx.symToHyp->Belt_MutableMapString.get(label)
let getFrame: (mmContext,string) => option<frame> = (ctx,label) => ctx.frames->Belt_MutableMapString.get(label)

let makeExpr: (mmContext,array<string>) => expr = (ctx, symbols) => {
    symbols->Js_array2.map(sym => {
        switch ctx.symToInt->Belt_MutableMapString.get(sym) {
            | None => raise(MmException({msg:`error in makeExpr: cannot find symbol '${sym}'`}))
            | Some(i) => i
        }
    })
}

let ctxExprToStr: (mmContext, expr) => array<string> = (ctx, expr) => {
    expr->Js_array2.map(i => if (i < 0) {ctx.consts[-i]} else {ctx.vars[i]})
}

let frameExprToStr: (mmContext, frame, expr) => array<string> = (ctx, frame, expr) => {
    expr->Js_array2.map(i => if (i < 0) {ctx.consts[-i]} else {frame.frameVarToSymb->Belt_MapInt.getExn(i)})
}

let getMandHyps:(mmContext, expr) => array<hypothesis> = (ctx, expr) => {
    let mandatoryVars: Belt_SetInt.t = extractMandatoryVariables(ctx, expr)
    extractMandatoryHypotheses(ctx, mandatoryVars)
}