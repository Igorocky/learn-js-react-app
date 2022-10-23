open MM_context
open MM_substitution
open MM_parser
open MM_syntax_proof_table

type proofPrecalcData = {
    frame: frame,
    frmConstParts:constParts,
    constParts:constParts,
    varGroups:array<varGroup>,
    subs:subs,
}

type exprSource =
    | Hypothesis({label:string})
    | Assertion({args:array<int>, label:string})

type proofRecord = {
    expr:expr,
    proved:bool,
    dist:int,
    src: option<array<exprSource>>
}

let isDirectFrame = frm => {
    let numOfVarsInAsrt = frm.asrt
        ->Js_array2.filter(i => i >= 0)
        ->Expln_utils_common.arrIntDistinct
        ->Js_array2.length
    numOfVarsInAsrt == frm.numOfVars
}

let prepareFrames = ctx => {
    let frames = []
    ctx->forEachFrame(frm => {
        if (isDirectFrame(frm)) {
            let frmConstParts = createConstParts(frm.asrt)
            let constParts = createMatchingConstParts(frmConstParts)
            let varGroups = createVarGroups(~frmExpr=frm.asrt, ~frmConstParts)
            let subs = {
                size: frm.numOfVars,
                begins: Belt_Array.make(frm.numOfVars, 0),
                ends: Belt_Array.make(frm.numOfVars, 0),
                exprs: Belt_Array.make(frm.numOfVars, []),
                isDefined: Belt_Array.make(frm.numOfVars, false),
            }
            frames->Js_array2.push({
                frame:frm,
                frmConstParts,
                constParts,
                varGroups,
                subs
            })->ignore
        }
    })
}

let findProof = (ctx, expr) => {
    let frames = prepareFrames(ctx)
    let tbl = createSyntaxProofTable(expr)
    let exprToProve = ref(tbl->getExprToProve)
    while (!tbl[0].proved && exprToProve.contents->Belt_Option.isSome) {
        switch exprToProve.contents {
            | Some(exprToProve) => {
                ()
            }
            | None => ()
        }
        exprToProve.contents = tbl->getExprToProve
    }
}