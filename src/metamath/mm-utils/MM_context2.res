open MM_parser
open MM_progress_tracker

// cdblk #types ===========================================================================================

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

type mutableMapStr<'v> = Belt.HashMap.String.t<'v>
type mutableMapInt<'v> = Belt.HashMap.Int.t<'v>
type mutableSetInt = Belt.HashSet.Int.t

type rec mmContext = {
    parent: option<mmContext>,
    consts: array<string>,
    varsBaseIdx: int,
    vars: array<string>,
    symToInt: mutableMapStr<int>,
    disj: mutableMapInt<mutableSetInt>,
    hypsBaseIdx: int,
    hyps: array<hypothesis>,
    symToHyp: mutableMapStr<hypothesis>,
    mutable lastComment: string,
    frames: mutableMapStr<frame>,
}

// cdblk #utils ===========================================================================================

let mutableMapStrMake = () => {
    Belt.HashMap.String.make(~hintSize=16)
}

let mutableMapStrPut = (map,k,v) => {
    map->Belt.HashMap.String.set(k,v)
}

let mutableMapStrHas = (map,k) => {
    map->Belt.HashMap.String.has(k)
}

let mutableMapStrGet = (map,k) => {
    map->Belt.HashMap.String.get(k)
}

let mutableMapStrForEach = (map,func) => {
    map->Belt.HashMap.String.forEach(func)
}

let mutableMapStrMakeFromArray = arr => {
    let map = mutableMapStrMake()
    arr->Js_array2.forEach(((k,v)) => map->mutableMapStrPut(k,v))
    map
}

let mutableMapIntMake = () => {
    Belt.HashMap.Int.make(~hintSize=16)
}

let mutableMapIntSize = (map) => {
    map->Belt.HashMap.Int.size
}

let mutableMapIntPut = (map,k,v) => {
    map->Belt.HashMap.Int.set(k,v)
}

let mutableMapIntHas = (map,k) => {
    map->Belt.HashMap.Int.has(k)
}

let mutableMapIntGet = (map,k) => {
    map->Belt.HashMap.Int.get(k)
}

let mutableMapIntForEach = (map,func) => {
    map->Belt.HashMap.Int.forEach(func)
}

let mutableMapIntMakeFromArray = arr => {
    let map = mutableMapIntMake()
    arr->Js_array2.forEach(((k,v)) => map->mutableMapIntPut(k,v))
    map
}

let mutableSetIntMake = () => {
    Belt.HashSet.Int.make(~hintSize=16)
}

let mutableSetIntSize = (set) => {
    set->Belt.HashSet.Int.size
}

let mutableSetIntAdd = (set,k) => {
    set->Belt.HashSet.Int.add(k)
}

let mutableSetIntHas = (set,k) => {
    set->Belt.HashSet.Int.has(k)
}

let mutableSetIntForEach = (set,func) => {
    set->Belt.HashSet.Int.forEach(func)
}

let mutableSetIntMakeFromArray = arr => {
    let set = mutableSetIntMake()
    arr->Js_array2.forEach(v => set->mutableSetIntAdd(v))
    set
}

let mutableSetIntToArray = set => {
    let arr = Expln_utils_common.createArray(set->mutableSetIntSize)
    let i = ref(0)
    set->mutableSetIntForEach(e => {
        arr[i.contents] = e
        i.contents = i.contents + 1
    })
    arr
}

let addDisjPairToMap = (disjMap:mutableMapInt<mutableSetInt>, n, m) => {
    switch disjMap->mutableMapIntGet(n) {
        | None => disjMap->mutableMapIntPut(n, mutableSetIntMakeFromArray([m]))
        | Some(set) => set->mutableSetIntAdd(m)
    }
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
// cdblk #search ===========================================================================================

let rec forEachCtxInDeclarationOrder = (ctx,consumer:mmContext=>option<'a>):option<'a> => {
    switch ctx.parent {
        | Some(parent) => {
            switch forEachCtxInDeclarationOrder(parent, consumer) {
                | Some(res) => Some(res)
                | None => consumer(ctx)
            }
        }
        | None => consumer(ctx)
    }
}

let rec forEachCtxInReverseOrder = (ctx,consumer:mmContext=>option<'a>):option<'a> => {
    switch consumer(ctx) {
        | Some(res) => Some(res)
        | None => {
            switch ctx.parent {
                | Some(parent) => forEachCtxInReverseOrder(parent, consumer)
                | None => None
            }
        }
    }
}

let rec isConst: (mmContext,string) => bool = (ctx, sym) => {
    switch ctx.parent {
        | Some(parent) => parent->isConst(sym)
        | None => ctx.symToInt->mutableMapStrGet(sym)->Belt_Option.map(i => i < 0)->Belt_Option.getWithDefault(false)
    }
}

let isVar: (mmContext,string) => bool = (ctx, sym) => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.symToInt->mutableMapStrGet(sym)
    })
        ->Belt_Option.map(i => 0 <= i)
        ->Belt_Option.getWithDefault(false)
}

let getHyp = (ctx,label):option<hypothesis> => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.symToHyp->mutableMapStrGet(label)
    })
}

let getFrame = (ctx,label):option<frame> => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.frames->mutableMapStrGet(label)
    })
}

let getTypeOfVar = (ctx,v):option<int> => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.hyps->Js_array2.find(hyp => hyp.typ == F && hyp.expr[1] == v)
    })->Belt_Option.map(hyp => hyp.expr[0])
}

let forEachHypothesisInDeclarationOrder: (mmContext, hypothesis => option<'a>) => option<'a> = (ctx, consumer) => {
    ctx->forEachCtxInDeclarationOrder(ctx => {
        Expln_utils_common.arrForEach(ctx.hyps, consumer)
    })
}

let symToInt = (ctx,sym) => {
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.symToInt->mutableMapStrGet(sym)
    })
}

let symToIntExn = (ctx,sym) => {
    switch symToInt(ctx,sym) {
        | Some(i) => i
        | None => raise(MmException({msg:`The symbol '${sym}' is not declared.`}))
    }
}

let makeExprExn: (mmContext,array<string>) => expr = (ctx, symbols) => {
    symbols->Js_array2.map(ctx->symToIntExn)
}

let ctxIntToStr = (ctx,i):option<string> => {
    if (i >= 0) {
        ctx->forEachCtxInReverseOrder(ctx => {
            if (i < ctx.varsBaseIdx) {
                None
            } else {
                Some(ctx.vars[i])
            }
        })
    } else {
        ctx->forEachCtxInDeclarationOrder(ctx => {
            ctx.consts->Belt_Array.get(-i)
        })
    }
}

let ctxIntToStrExn = (ctx,i) => {
    ctxIntToStr(ctx,i)->Belt.Option.getExn
}

let ctxExprToStrExn: (mmContext, expr) => string = (ctx, expr) => {
    expr->Js_array2.map(ctxIntToStrExn(ctx, _))->Expln_utils_common.strJoin(~sep=" ", ())
}

let frmIntToStrExn: (mmContext, frame, int) => string = (ctx, frame, i) => {
    if (i < 0) {ctx->ctxIntToStrExn(i)} else {frame.frameVarToSymb->Belt_MapInt.getExn(i)}
}

let frmExprToStrExn: (mmContext, frame, expr) => string = (ctx, frame, expr) => {
    expr->Js_array2.map(frmIntToStrExn(ctx, frame, _))->Expln_utils_common.strJoin(~sep=" ", ())
}

let extractMandatoryVariables = (ctx,asrt): mutableSetInt => {
    let res = mutableSetIntMake()
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == E) {
            hyp.expr->Js_array2.forEach(i => if i >= 0 {res->mutableSetIntAdd(i)})
        }
        None
    })->ignore
    asrt->Js_array2.forEach(i => if i >= 0 {res->mutableSetIntAdd(i)})
    res
}

let extractMandatoryDisj = (ctx, mandatoryVars:mutableSetInt): mutableMapInt<mutableSetInt> => {
    let mandatoryDisj = mutableMapIntMake()
    ctx->forEachCtxInReverseOrder(ctx => {
        ctx.disj->mutableMapIntForEach((n,ms) => {
            if (mandatoryVars->mutableSetIntHas(n)) {
                ms->mutableSetIntForEach(m => {
                    if (mandatoryVars->mutableSetIntHas(m)) {
                        addDisjPairToMap(mandatoryDisj, n, m)
                    }
                })
            }
        })->ignore
        None
    })->ignore
    mandatoryDisj
}

let extractMandatoryHypotheses = (ctx, mandatoryVars:mutableSetInt):array<hypothesis> => {
    let res = []
    ctx->forEachCtxInDeclarationOrder(ctx=>{
        ctx.hyps->Js.Array2.forEach(hyp => {
            if (hyp.typ == E || hyp.typ == F && mandatoryVars->mutableSetIntHas(hyp.expr[1])) {
                res->Js.Array2.push(hyp)->ignore
            }
        })
        None
    })->ignore
    res
}

let getMandHyps:(mmContext, expr) => array<hypothesis> = (ctx, expr) => {
    let mandatoryVars: mutableSetInt = extractMandatoryVariables(ctx, expr)
    extractMandatoryHypotheses(ctx, mandatoryVars)
}

let forEachFrame: (mmContext, frame => option<'a>) => option<'a> = (ctx, consumer) => {
    ctx->forEachCtxInDeclarationOrder(ctx => {
        let result = ref(None)
        ctx.frames->mutableMapStrForEach((_,frm) => {
            if (result.contents->Belt_Option.isNone) {
                result.contents = consumer(frm)
            }
        })
        result.contents
    })
}

let rec getNestingLevel: mmContext => int = ctx => {
    switch ctx.parent {
        | None => 0
        | Some(pCtx) => 1 + getNestingLevel(pCtx)
    }
}

let findParentheses: (mmContext, ~onProgress:float=>unit=?, unit) => array<int> = (ctx, ~onProgress=?, ()) => {
    let getAllConsts = ctx => {
        let allConsts = ["(", ")", "[", "]", "{", "}"]->Js.Array2.filter(ctx->isConst)->makeExprExn(ctx, _)
        let predefiend = Belt_SetInt.fromArray(allConsts)
        ctx->forEachCtxInDeclarationOrder(ctx => {
            ctx.consts->Js_array2.forEach(cStr => {
                if (cStr != "") {
                    switch ctx.symToInt->mutableMapStrGet(cStr) {
                        | None => raise(MmException({msg:`Cannot determine int code for constant symbol '${cStr}'`}))
                        | Some(i) => {
                            if !(predefiend->Belt_SetInt.has(i)) {
                                allConsts->Js_array2.push(i)->ignore
                            }
                        }
                    }
                }
            })
            Some(())
        })->ignore
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
    let maxCF = maxC->Belt_Int.toFloat
    let foundParens = []
    let progressState = ref(progressTrackerMake(~step=1., ~onProgress?, ()))
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
        progressState.contents = progressState.contents->progressTrackerSetCurrPct(
            c.contents->Belt_Int.toFloat /. maxCF
        )
    }

    foundParens
}
// cdblk #update ===========================================================================================

let createContext: (~parent:mmContext=?, ()) => mmContext = (~parent=?, ()) => {
    parent,
    consts: [""],
    varsBaseIdx: switch parent {
        | None => 0
        | Some(parent) => parent.varsBaseIdx + parent.vars->Js_array2.length
    },
    vars: [],
    symToInt: mutableMapStrMake(),
    disj: mutableMapIntMake(),
    hypsBaseIdx: switch parent {
        | None => 0
        | Some(parent) => parent.hypsBaseIdx + parent.hyps->Js_array2.length
    },
    hyps: [],
    symToHyp: mutableMapStrMake(),
    lastComment: "",
    frames: mutableMapStrMake(),
}

let openChildContext: mmContext => mmContext = ctx => {
    createContext(~parent=ctx, ())
}

let closeChildContext: mmContext => mmContext = ctx => {
    switch ctx.parent {
        | None => raise(MmException({msg:`Cannot close the root context.`}))
        | Some(parent) => {
            ctx.frames->mutableMapStrForEach((k,v) => parent.frames->mutableMapStrPut(k,v))
            parent
        }
    }
}

let addComment: (mmContext,string) => unit = (ctx,str) => {
    ctx.lastComment = str
}

let addConst: (mmContext,string) => unit = (ctx,cName) => {
    if (ctx.parent->Belt_Option.isSome) {
        raise(MmException({msg:`An attempt to declare a constant '${cName}' in an inner block.`}))
    } else if (isConst(ctx, cName) || isVar(ctx, cName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${cName}' as a constant.`}))
    } else {
        ctx.symToInt->mutableMapStrPut(cName, -(ctx.consts->Js_array2.length))
        ctx.consts->Js_array2.push(cName)->ignore
    }
}

let addVar: (mmContext,string) => unit = (ctx,vName) => {
    if (isConst(ctx, vName) || isVar(ctx, vName)) {
        raise(MmException({msg:`An attempt to re-declare the math symbol '${vName}' as a variable.`}))
    } else {
        ctx.symToInt->mutableMapStrPut(vName, ctx.varsBaseIdx + ctx.vars->Js_array2.length)
        ctx.vars->Js_array2.push(vName)->ignore
    }
}

let addDisjPair = (ctx, n, m) => addDisjPairToMap(ctx.disj,n,m)

let addDisj: (mmContext,array<string>) => unit = (ctx, vars) => {
    switch vars->Js_array2.find(sym => !(ctx->isVar(sym))) {
        | Some(sym) => raise(MmException({msg:`The symbol '${sym}' is not a variable but it is used in a disjoint statement.`}))
        | None => {
            let varInts = vars->Js_array2.map(ctx->symToIntExn)
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
    } else if (ctx->getHyp(label)->Belt_Option.isSome || ctx->getFrame(label)->Belt_Option.isSome) {
        raise(MmException({msg:`Cannot reuse the label '${label}'`}))
    } else {
        let varName = exprStr[1]
        let varInt = ctx->symToIntExn(varName)
        if (ctx->getTypeOfVar(varInt)->Belt_Option.isSome) {
            raise(MmException({msg:`Cannot redefine typecode for the variable '${varName}'`}))
        } else {
            let expr = makeExprExn(ctx, exprStr)
            let hyp = {typ:F, label, expr}
            ctx.hyps->Js_array2.push(hyp)->ignore
            ctx.symToHyp->mutableMapStrPut(label, hyp)
        }
    }
}

let addEssential: (mmContext, ~label:string, ~exprStr:array<string>) => unit = (ctx, ~label, ~exprStr) => {
    if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an essential expression must be at least 1.`}))
    } else if (!(ctx->isConst(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an essential expression must be a constant.`}))
    } else {
        let expr = makeExprExn(ctx, exprStr)
        let hyp = {typ:E, label, expr}
        ctx.hyps->Js_array2.push(hyp)->ignore
        ctx.symToHyp->mutableMapStrPut(label, hyp)
   }
}

let renumberVarsInDisj = (disj: mutableMapInt<mutableSetInt>, renumbering: Belt_MapInt.t<int>): Belt_MapInt.t<Belt_SetInt.t> => {
    let disjArr = Expln_utils_common.createArray(disj->mutableMapIntSize)
    disj->mutableMapIntForEach((n,ms) => {
        let msArr = Expln_utils_common.createArray(ms->mutableSetIntSize)
        let i = ref(0)
        ms->mutableSetIntForEach(m => {
            msArr[i.contents] = renumbering->Belt_MapInt.getExn(m)
            i.contents = i.contents + 1
        })
        disjArr->Js.Array2.push((renumbering->Belt_MapInt.getExn(n), Belt_Set.Int.fromArray(msArr)))->ignore
    })
    Belt_MapInt.fromArray(disjArr)
}

let renumberVarsInExpr = (expr: expr, renumbering: Belt_MapInt.t<int>): expr => {
    expr->Js_array2.map(i => if (i<0) {i} else {renumbering->Belt_MapInt.getExn(i)})
}

let renumberVarsInHypothesis = (hyp: hypothesis, renumbering: Belt_MapInt.t<int>): hypothesis => {
    ...hyp,
    expr: renumberVarsInExpr(hyp.expr, renumbering)
}

let createFrameVarToSymbMap = (ctx, mandatoryHypotheses:array<hypothesis>, asrt, renumbering: Belt_MapInt.t<int>): Belt_MapInt.t<string> => {
    let allVars = mutableSetIntMake()
    mandatoryHypotheses->Js.Array2.forEach(hyp => {
        hyp.expr->Js_array2.forEach(i => {
            if (i >= 0) {
                allVars->mutableSetIntAdd(i)
            }
        })
    })
    asrt->Js_array2.forEach(i => {
        if (i >= 0) {
            allVars->mutableSetIntAdd(i)
        }
    })
    Belt_MapInt.fromArray(
        allVars->mutableSetIntToArray->Js_array2.map(v => (renumbering->Belt_MapInt.getExn(0), ctxIntToStrExn(ctx,v)))
    )
}

let extractVarTypes = (mandatoryHypotheses:array<hypothesis>, renumbering: Belt_MapInt.t<int>): array<int> => {
    let varTypes = Expln_utils_common.createArray(renumbering->Belt_MapInt.size)
    mandatoryHypotheses->Js_array2.forEach(hyp => {
        if (hyp.typ == F) {
            varTypes[renumbering->Belt_MapInt.getExn(hyp.expr[1])] = hyp.expr[0]
        }
    })
    varTypes
}

let createFrame: (mmContext, string, array<string>) => frame = (ctx, label, exprStr) => {
    if (label->Js_string2.trim == "") {
        raise(MmException({msg:`Cannot use empty string as a label.`}))
    } else if (ctx->getHyp(label)->Belt_Option.isSome || ctx->getFrame(label)->Belt_Option.isSome) {
        raise(MmException({msg:`Cannot reuse the label '${label}'`}))
    } else if (exprStr->Js_array2.length < 1) {
        raise(MmException({msg:`Length of an assertion expression must be at least 1.`}))
    } else if (!(ctx->isConst(exprStr[0]))) {
        raise(MmException({msg:`The first symbol in an assertion expression must be a constant.`}))
    } else {
        switch exprStr->Js_array2.find(sym => ctx->symToInt(sym)->Belt_Option.isNone) {
            | Some(sym) => raise(MmException({msg:`The symbol '${sym}' must be either a constant or a variable.`}))
            | None => {
                let asrt: expr = exprStr->Js_array2.map(symToIntExn(ctx, _))
                let mandatoryVars: mutableSetInt = extractMandatoryVariables(ctx, asrt)
                let mandatoryDisj: mutableMapInt<mutableSetInt> = extractMandatoryDisj(ctx, mandatoryVars)
                let mandatoryHypotheses: array<hypothesis> = extractMandatoryHypotheses(ctx, mandatoryVars)
                let varRenumbering: Belt_MapInt.t<int> = mandatoryVars
                                                            ->mutableSetIntToArray
                                                            ->Js_array2.mapi((v,i) => (v,i))
                                                            ->Belt_MapInt.fromArray
                let varTypes = extractVarTypes(mandatoryHypotheses, varRenumbering)
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
    ctx.frames->mutableMapStrPut(label, createFrame(ctx, label, exprStr))
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
    let ctx = ref(
        switch initialContext {
            | Some(ctx) => ctx
            | None => createContext(())
        }
    )
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
                        ctx.contents = openChildContext(ctx.contents)
                    }
                    None
                }
                | {stmt:Axiom({label}) | Provable({label})} => {
                    onAsrtProcess()
                    if (stopBefore == label) {
                        Some(())
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
                | {stmt} => applySingleStmt(ctx.contents,stmt)
            }
            None
        },
        ~postProcess = (_,node) => {
            switch node {
                | {stmt:Block({level})} => {
                    if (level > 0) {
                        ctx.contents = closeChildContext(ctx.contents)
                    }
                    None
                }
                | {stmt:Axiom({label}) | Provable({label})} if stopAfter == label => Some(())
                | _ => None
            }
        },
        ()
    )->ignore
    ctx.contents
}