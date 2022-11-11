open MM_parser

// code_block #types ===========================================================================================

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

// code_block #utils ===========================================================================================

let mutableMapStrMake = () => {
    Belt.HashMap.String.make(~hintSize=1000)
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
    Belt.HashMap.Int.make(~hintSize=1000)
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
    Belt.HashSet.Int.make(~hintSize=1000)
}

let mutableSetIntAdd = (set,k) => {
    set->Belt.HashSet.Int.add(k)
}

let mutableSetIntHas = (set,k) => {
    set->Belt.HashSet.Int.has(k)
}

let mutableSetIntMakeFromArray = arr => {
    let set = mutableSetIntMake()
    arr->Js_array2.forEach(v => set->mutableSetIntAdd(v))
    set
}

let addDisjPairToMap = (disjMap:mutableMapInt<mutableSetInt>, n, m) => {
    switch disjMap->mutableMapIntGet(n) {
        | None => disjMap->mutableMapIntPut(n, mutableSetIntMakeFromArray([m]))
        | Some(set) => set->mutableSetIntAdd(m)
    }
}

let rec symToIntExn = (ctx,sym) => {
    switch ctx.symToInt->mutableMapStrGet(sym) {
        | Some(i) => i
        | None => {
            switch ctx.parent {
                | Some(parent) => symToIntExn(parent, sym)
                | None => raise(MmException({msg:`The symbol '${sym}' is not declared.`}))
            }
        }
    }
}

let makeExprExn: (mmContext,array<string>) => expr = (ctx, symbols) => {
    symbols->Js_array2.map(ctx->symToIntExn)
}

// code_block #search ===========================================================================================

let rec isConst: (mmContext,string) => bool = (ctx, sym) => {
    switch ctx.parent {
        | Some(parent) => parent->isConst(sym)
        | None => ctx.symToInt->mutableMapStrGet(sym)->Belt_Option.map(i => i < 0)->Belt_Option.getWithDefault(false)
    }
}

let rec isVar: (mmContext,string) => bool = (ctx, sym) => {
    switch ctx.symToInt->mutableMapStrGet(sym) {
        | Some(i) => i >= 0
        | None => {
            switch ctx.parent {
                | None => false
                | Some(parent) => parent->isVar(sym)
            }
        }
    }
}

let rec getHyp = (ctx,label):option<hypothesis> => {
    switch ctx.symToHyp->mutableMapStrGet(label) {
        | Some(hyp) => Some(hyp)
        | None => {
            switch ctx.parent {
                | Some(parent) => getHyp(parent, label)
                | None => None
            }
        }
    }
}

let rec getFrame = (ctx,label):option<frame> => {
    switch ctx.frames->mutableMapStrGet(label) {
        | Some(frm) => Some(frm)
        | None => {
            switch ctx.parent {
                | Some(parent) => getFrame(parent, label)
                | None => None
            }
        }
    }
}

let rec getTypeOfVar = (ctx,v):option<int> => {
    switch ctx.hyps->Js_array2.find(hyp => hyp.typ == F && hyp.expr[1] == v) {
        | Some(hyp) => Some(hyp.expr[0])
        | None => {
            switch ctx.parent {
                | Some(parent) => getTypeOfVar(parent, v)
                | None => None
            }
        }
    }
}
// code_block #update ===========================================================================================

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
/*




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
*/