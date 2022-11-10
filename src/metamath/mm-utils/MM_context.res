open MM_parser

type expr = array<int>

type hypothesisType = F | E

type hypothesis = {
    typ: hypothesisType,
    label: string,
    expr: expr
}

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
    mutable symToHyp: Belt.MutableMap.String.t<hypothesis>,
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

let makeExpr: (mmContext,array<string>) => expr = (ctx, symbols) => {
    symbols->Js_array2.map(sym => {
        switch ctx.symToInt->Belt_MutableMapString.get(sym) {
            | None => raise(MmException({msg:`error in makeExpr: cannot find symbol '${sym}'`}))
            | Some(i) => i
        }
    })
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
        if (ctx.hyps->Js_array2.some(hyp => hyp.typ == F && hyp.expr[1] == varInt)) {
            raise(MmException({msg:`Cannot redefine typecode for the variable '${varName}'`}))
        } else {
            let expr = makeExpr(ctx, exprStr)
            let hyp = {typ:F, label, expr}
            ctx.hyps->Js_array2.push(hyp)->ignore
            ctx.symToHyp->Belt_MutableMapString.set(label, hyp)
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
                let expr = makeExpr(ctx, exprStr)
                let hyp = {typ:E, label, expr}
                ctx.hyps->Js_array2.push(hyp)->ignore
                ctx.symToHyp->Belt_MutableMapString.set(label, hyp)
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
                ->Js_array2.filter(hyp => hyp.typ == E)
                ->Js_array2.map(hyp => extractVarsFromExpr(hyp.expr))
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
        ->Js_array2.filter(hyp => hyp.typ == E || hyp.typ == F && mandatoryVars->Belt_SetInt.has(hyp.expr[1]))
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
    ...hyp,
    expr: renumberVarsInExpr(hyp.expr, renumbering)
}

let createFrameVarToSymbMap = (ctx, mandatoryHypotheses:array<hypothesis>, asrt, varRenumbering: Belt_MapInt.t<int>): Belt_MapInt.t<string> => {
    asrt->Js_array2.concatMany(
        mandatoryHypotheses->Js_array2.map(hyp => hyp.expr)
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
        if (hyp.typ == F && mandatoryVars->Belt_SetInt.has(hyp.expr[1])) {
            varTypes[varRenumbering->Belt_MapInt.getExn(hyp.expr[1])] = hyp.expr[0]
        }
    })
    varTypes
}

let createFrame: (mmContext, string, array<string>) => frame = (ctx, label, exprStr) => {
    if (label->Js_string2.trim == "") {
        raise(MmException({msg:`Cannot use empty string as a label.`}))
    } else if (ctx.frames->Belt_MutableMapString.has(label)) {
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
    ctx.frames->Belt_MutableMapString.set(label, createFrame(ctx, label, exprStr))
}

let applySingleStmt = (ctx:mmContext, stmt:stmt):unit => {
    switch stmt {
        | Comment({text}) => addComment(ctx, text)
        | Const({symbols}) => symbols->Js_array2.forEach(addConst(ctx, _))
        | Block(_) => raise(MmException({msg:`Block statements are not accepted by applySingleStmt().`}))
        | Var({symbols}) => symbols->Js_array2.forEach(addVar(ctx, _))
        | Disj({vars}) => addDisj(ctx, vars)
        | Floating({label, expr}) => addFloating(ctx, ~label, ~exprStr=expr)
        | Essential({label, expr}) => addEssential(ctx, ~label, ~exprStr=expr)
        | Axiom({label, expr}) | Provable({label, expr}) => addAssertion(ctx, ~label, ~exprStr=expr)
    }
}

let loadContext: (mmAstNode, ~initialContext:mmContext=?, ~stopBefore: string=?, ~stopAfter: string=?, 
                    ~expectedNumOfAssertions:int=?, ~onProgress:float=>unit=?, ()) => mmContext =
                                                (ast, ~initialContext=?,~stopBefore="",~stopAfter="", 
                                                    ~expectedNumOfAssertions=-1, ~onProgress= _=>(), ()) => {
    let ctx = initialContext->Belt_Option.getWithDefault(createEmptyContext())
    let lastPctSent = ref(-1)
    let labelsProcessed = ref(0.)
    let expectedNumOfAssertionsF = expectedNumOfAssertions->Belt_Int.toFloat

    let onAsrtProcess = () => {
        if (expectedNumOfAssertions > 0) {
            labelsProcessed.contents = labelsProcessed.contents +. 1.
            let pct = (labelsProcessed.contents /. expectedNumOfAssertionsF *. 100.)->Js_math.round->Belt_Float.toInt
            if (lastPctSent.contents < pct) {
                onProgress(pct->Belt_Int.toFloat /. 100.)
                lastPctSent.contents = pct
            }
        }
    }

    traverseAst(
        (),
        ast,
        ~preProcess = (_,node) => {
            switch node {
                | {stmt:Block({level})} => {
                    if (level > 0) {
                        openChildContext(ctx)
                    }
                    None
                }
                | {stmt:Axiom({label}) | Provable({label})} => {
                    onAsrtProcess()
                    if (stopBefore == label) {
                        Some(ctx)
                    } else {
                        None
                    }
                }
                | _ => None
            }
        },
        ~process = (_,node) => {
            switch node {
                | {stmt:Block(_)} => ()
                | {stmt} => applySingleStmt(ctx,stmt)
            }
            None
        },
        ~postProcess = (_,node) => {
            switch node {
                | {stmt:Block({level})} => {
                    if (level > 0) {
                        closeChildContext(ctx)
                    }
                    None
                }
                | {stmt:Axiom({label}) | Provable({label})} if stopAfter == label => Some(ctx)
                | _ => None
            }
        },
        ()
    )->ignore
    ctx
}

let getHypothesis: (mmContext,string) => option<hypothesis> = (ctx,label) => ctx.symToHyp->Belt_MutableMapString.get(label)
let getFrame: (mmContext,string) => option<frame> = (ctx,label) => ctx.frames->Belt_MutableMapString.get(label)

let ctxIntToStrExn: (mmContext, int) => string = (ctx, i) => {
    if (i < 0) {ctx.consts[-i]} else {ctx.vars[i]}
}

let ctxExprToStrExn: (mmContext, expr) => string = (ctx, expr) => {
    expr->Js_array2.map(ctxIntToStrExn(ctx, _))->Expln_utils_common.strJoin(~sep=" ", ())
}

let frmIntToStrExn: (mmContext, frame, int) => string = (ctx, frame, i) => {
    if (i < 0) {ctx.consts[-i]} else {frame.frameVarToSymb->Belt_MapInt.getExn(i)}
}

let frmExprToStrExn: (mmContext, frame, expr) => string = (ctx, frame, expr) => {
    expr->Js_array2.map(frmIntToStrExn(ctx, frame, _))->Expln_utils_common.strJoin(~sep=" ", ())
}

let getMandHyps:(mmContext, expr) => array<hypothesis> = (ctx, expr) => {
    let mandatoryVars: Belt_SetInt.t = extractMandatoryVariables(ctx, expr)
    extractMandatoryHypotheses(ctx, mandatoryVars)
}

let forEachFrame: (mmContext, frame => option<'a>) => option<'a> = (ctx, consumer) => {
    let result = ref(None)
    ctx.frames->Belt_MutableMapString.forEach((_,frm) => {
        if (result.contents->Belt_Option.isNone) {
            result.contents = consumer(frm)
        }
    })
    result.contents
}

let forEachHypothesis: (mmContext, hypothesis => option<'a>) => option<'a> = (ctx, consumer) => {
    ctx.hyps->Expln_utils_common.arrForEach(consumer)
}

let exprEq: (expr,expr) => bool = (a,b) => {
    let len = a->Js_array2.length
    let eq = ref(len == b->Js_array2.length)
    let i = ref(0)
    while (eq.contents && i.contents < len) {
        eq.contents = a[i.contents] == b[i.contents]
        i.contents = i.contents + 1
    }
    eq.contents
}

let rec getNestingLevel: mmContext => int = ctx => {
    switch ctx.parent {
        | None => 0
        | Some(pCtx) => 1 + getNestingLevel(pCtx)
    }
}

let findParentheses: mmContext => array<int> = ctx => {
    let getAllConsts = ctx => {
        let allConsts = ["(", ")", "[", "]", "{", "}"]->Js.Array2.filter(ctx->isConst)->makeExpr(ctx, _)
        let predefiend = Belt_SetInt.fromArray(allConsts)
        ctx.consts->Js_array2.forEach(cStr => {
            if (cStr != "") {
                switch ctx.symToInt->Belt_MutableMapString.get(cStr) {
                    | None => raise(MmException({msg:`Cannot determine int code for constant symbol '${cStr}'`}))
                    | Some(i) if !(predefiend->Belt_SetInt.has(i)) => allConsts->Js_array2.push(i)->ignore
                    | _ => ()
                }
            }
        })
        allConsts
    }

    let getAllExprs = ctx => {
        let allExpr = []
        ctx->forEachFrame(frame => {
            frame.hyps->Js_array2.forEach(hyp => {
                if (hyp.typ == E) {
                    allExpr->Js_array2.push(hyp.expr)->ignore
                }
            })
            allExpr->Js_array2.push(frame.asrt)->ignore
            None
        })->ignore
        allExpr
    }

    let checkValidParens = (allExprs, openSym, closeSym) => {
        open MM_parenCounter
        let res = ref(true)
        let openUsed = ref(false)
        let closeUsed = ref(false)
        let parenCnt = parenCntMake([openSym, closeSym])
        let parenState = ref(Balanced)
        let allExprsLen = allExprs->Js_array2.length
        let e = ref(0)
        while (e.contents < allExprsLen && res.contents) {
            let expr = allExprs[e.contents]
            let exprLen = expr->Js_array2.length
            let s = ref(0)
            while (s.contents < exprLen && res.contents) {
                let sym = expr[s.contents]
                if (!openUsed.contents && sym == openSym) {
                    openUsed.contents = true
                }
                if (!closeUsed.contents && sym == closeSym) {
                    closeUsed.contents = true
                }
                parenState.contents = parenCnt->parenCntPut(sym)
                res.contents = parenState.contents != Failed
                s.contents = s.contents + 1
            }
            res.contents = parenState.contents == Balanced
            e.contents = e.contents + 1
        }
        res.contents && openUsed.contents && closeUsed.contents
    }

    let allConsts = getAllConsts(ctx)
    let allExprs = getAllExprs(ctx)

    let c = ref(0)
    let maxC = allConsts->Js_array2.length - 2
    let foundParens = []
    while (c.contents <= maxC) {
        let openSym = allConsts[c.contents]
        let closeSym = allConsts[c.contents+1]
        if (checkValidParens(allExprs, openSym, closeSym)) {
            foundParens->Js_array2.push(openSym)->ignore
            foundParens->Js_array2.push(closeSym)->ignore
            c.contents = c.contents + 2
        } else {
            c.contents = c.contents + 1
        }
    }

    foundParens
}