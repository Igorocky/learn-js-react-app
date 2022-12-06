open MM_substitution
open MM_context
open MM_parenCounter
open MM_parser

type labeledExpr = {
    label:string,
    expr:expr
}

type applyAssertionResult = {
    workVars: array<int>,
    workVarTypes: array<int>,
    disj:disjMutable,
    argLabels: array<option<string>>,
    argExprs: array<option<expr>>,
    asrtLabel: string,
    asrtExpr: expr,
}

let rec iterateCombinationsRec = (
    ~candidatesPerHyp:array<array<int>>,
    ~comb:array<int>,
    ~hypIdx:int,
    ~combinationConsumer:array<int>=>contunieInstruction,
):contunieInstruction => {
    if (hypIdx == comb->Js.Array2.length) {
        combinationConsumer(comb)
    } else {
        let res = ref(Continue)
        let c = ref(0)
        let maxC = candidatesPerHyp[hypIdx]->Js.Array2.length-1
        while (res.contents == Continue && c.contents <= maxC) {
            comb[hypIdx] = candidatesPerHyp[hypIdx][c.contents]
            res.contents = iterateCombinationsRec(
                ~candidatesPerHyp,
                ~comb,
                ~hypIdx = hypIdx+1,
                ~combinationConsumer
            )
            c.contents = c.contents + 1
        }
        res.contents
    }
}

let iterateCombinations = (
    ~numOfStmts:int,
    ~numOfHyps:int,
    ~stmtCanMatchHyp:(int,int)=>bool,
    ~combinationConsumer:array<int>=>contunieInstruction,
):contunieInstruction => {
    let candidatesPerHyp = Belt_Array.makeBy(numOfHyps, _=>[])
    let maxH = numOfHyps-1
    let maxS = numOfStmts-1
    for h in 0 to maxH {
        for s in -1 to maxS {
            if (stmtCanMatchHyp(s,h)) {
                candidatesPerHyp[h]->Js_array2.push(s)->ignore
            }
        }
    }
    let thereIsHypWithoutAnyCandidate = candidatesPerHyp->Js_array2.some(candidates => candidates->Js_array2.length == 0)
    if (!thereIsHypWithoutAnyCandidate) {
        let comb = Belt_Array.make(numOfHyps, 0)
        iterateCombinationsRec(
            ~candidatesPerHyp,
            ~comb,
            ~hypIdx = 0,
            ~combinationConsumer
        )
    } else {
        Continue
    }
}

let stmtCanMatchHyp = (
    ~frm:frameProofDataRec,
    ~hypIdx:int,
    ~stmt:expr,
    ~hyp:expr,
    ~parenCnt:parenCnt,
):bool => {
    let res = ref(false)
    iterateSubstitutions(
        ~frmExpr = hyp,
        ~expr = stmt,
        ~frmConstParts = frm.frmConstParts[hypIdx], 
        ~constParts = frm.constParts[hypIdx], 
        ~varGroups = frm.varGroups[hypIdx],
        ~subs = frm.subs,
        ~parenCnt,
        ~consumer = subs => {
            res.contents = true
            Stop
        }
    )->ignore
    res.contents
}

let iterateSubstitutionsWithWorkVars = (
    ~frm:frameProofDataRec,
    ~hypIdx: int,
    ~consumer: frameProofDataRec => contunieInstruction
):contunieInstruction => {
    let initialNumOfWorkVars = frm.workVars.vars->Js_array2.length
    let predefinedSubs = frm.subs.isDefined->Js_array2.copy

    let nextVar = ref(frm.workVars.numOfCtxVars + frm.workVars.vars->Js_array2.length)
    let frmVars = []
    let newVars = []
    let newVarTypes = []
    let newExprWithWorkVars = applySubs(
        ~frmExpr = if (hypIdx < frm.hypsE->Js.Array2.length) {frm.hypsE[hypIdx].expr} else {frm.frame.asrt},
        ~subs=frm.subs,
        ~createWorkVar = frmVar => {
            switch frmVars->Js_array2.indexOf(frmVar) {
                | -1 => {
                    let newVar = nextVar.contents
                    nextVar.contents = nextVar.contents + 1
                    frmVars->Js_array2.push(frmVar)->ignore
                    newVars->Js_array2.push(newVar)->ignore
                    newVarTypes->Js_array2.push(frm.frame.varTypes[frmVar])->ignore
                    newVar
                }
                | idx => newVars[idx]
            }
        }
    )
    let maxI = frmVars->Js_array2.length - 1
    for i in 0 to maxI {
        let frmVar = frmVars[i]
        let workVar = newVars[i]
        let workVarType = newVarTypes[i]

        frm.subs.exprs[frmVar] = [workVar]
        frm.subs.begins[frmVar] = 0
        frm.subs.ends[frmVar] = 0
        frm.subs.isDefined[frmVar] = true

        frm.workVars.vars->Js_array2.push(workVar)->ignore
        frm.workVars.types->Js_array2.push(workVarType)->ignore
    }
    frm.workVars.hypIdxToExprWithWorkVars[hypIdx] = Some(newExprWithWorkVars)

    let res = consumer(frm)

    predefinedSubs->Js_array2.forEachi((predefined,i) => frm.subs.isDefined[i]=predefined)
    frm.workVars.vars->Js_array2.removeFromInPlace(~pos=initialNumOfWorkVars)->ignore
    frm.workVars.types->Js_array2.removeFromInPlace(~pos=initialNumOfWorkVars)->ignore
    frm.workVars.hypIdxToExprWithWorkVars[hypIdx] = None

    res
}

let rec iterateSubstitutionsForHyps = (
    ~frm:frameProofDataRec,
    ~parenCnt:parenCnt,
    ~statements:array<labeledExpr>,
    ~comb:array<int>,
    ~hypIdx:int,
    ~onMatchFound: frameProofDataRec => contunieInstruction
):contunieInstruction => {
    if (hypIdx == comb->Js.Array2.length) {
        iterateSubstitutionsWithWorkVars(
            ~frm,
            ~hypIdx,
            ~consumer = onMatchFound
        )
    } else if (comb[hypIdx] >= 0) {
        iterateSubstitutions(
            ~frmExpr = frm.hypsE[hypIdx].expr,
            ~expr = statements[comb[hypIdx]].expr,
            ~frmConstParts = frm.frmConstParts[hypIdx], 
            ~constParts = frm.constParts[hypIdx], 
            ~varGroups = frm.varGroups[hypIdx],
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = subs => {
                iterateSubstitutionsForHyps(
                    ~frm,
                    ~parenCnt,
                    ~statements,
                    ~comb,
                    ~hypIdx = hypIdx+1,
                    ~onMatchFound
                )
            }
        )
    } else {
        iterateSubstitutionsWithWorkVars(
            ~frm,
            ~hypIdx,
            ~consumer = frm => {
                iterateSubstitutionsForHyps(
                    ~frm,
                    ~parenCnt,
                    ~statements,
                    ~comb,
                    ~hypIdx = hypIdx+1,
                    ~onMatchFound
                )
            }
        )
    }
}

// let checkTypesInFloatingHyps = (
//     ~ctx:mmContext, 
//     ~frmsSyntax:frameProofData, 
//     ~frm:frameProofDataRec,
//     ~parenCnt:parenCnt, 
//     ~proofTbl:proofTable
// ) => {

// }

let extractNewDisj = (~ctx, ~frmDisj:Belt_MapInt.t<Belt_SetInt.t>, ~subs:subs, ~maxCtxVar:int):option<disjMutable> => {
    let result = disjMutableMake()
    let disjIsValid = verifyDisjoints(~frmDisj, ~subs, ~isDisjInCtx = (n,m) => {
        if (n <= maxCtxVar && m <= maxCtxVar) {
            ctx->isDisj(n,m)
        } else {
            result->addDisjPairToMap(n,m)
            true
        }
    })
    if (disjIsValid) {
        Some(result)
    } else {
        None
    }
}

let iterateSubstitutionsForResult = (
    ~frm:frameProofDataRec,
    ~result:option<expr>,
    ~parenCnt:parenCnt,
    ~consumer:frameProofDataRec=>contunieInstruction,
):contunieInstruction => {
    switch result {
        | None => consumer(frm)
        | Some(expr) => {
            iterateSubstitutions(
                ~frmExpr = frm.frame.asrt,
                ~expr,
                ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                ~constParts = frm.constParts[frm.numOfHypsE], 
                ~varGroups = frm.varGroups[frm.numOfHypsE],
                ~subs = frm.subs,
                ~parenCnt,
                ~consumer = subs => {
                    consumer(frm)
                }
            )
        }
    }
}

let applyAssertions = (
    ~ctx,
    ~frms:frameProofData,
//    ~frmsSyntax:frameProofData,
    ~nonSyntaxTypes:array<int>,
    ~statements:array<labeledExpr>,
    ~result:option<expr>=?,
    ~parenCnt:parenCnt,
    ~frameFilter:frame=>bool=_=>true,
    ~onMatchFound:applyAssertionResult=>contunieInstruction,
    ()
):unit => {
    let numOfStmts = statements->Js_array2.length
    frms->Js_array2.forEach(frm => {
        if (nonSyntaxTypes->Js_array2.includes(frm.frame.asrt[0]) && frameFilter(frm.frame)) {
            let numOfHyps = frm.hypsE->Js_array2.length
            iterateSubstitutionsForResult(
                ~frm,
                ~result,
                ~parenCnt,
                ~consumer = frm => {
                    iterateCombinations(
                        ~numOfStmts,
                        ~numOfHyps,
                        ~stmtCanMatchHyp = (s,h) => {
                            if (s == -1) {
                                true
                            } else {
                                stmtCanMatchHyp(
                                    ~frm,
                                    ~hypIdx=h,
                                    ~stmt = statements[s].expr,
                                    ~hyp = frm.hypsE[h].expr,
                                    ~parenCnt,
                                )
                            }
                        },
                        ~combinationConsumer = comb => {
                            iterateSubstitutionsForHyps(
                                ~frm,
                                ~parenCnt,
                                ~statements,
                                ~comb,
                                ~hypIdx=0,
                                ~onMatchFound = frm => {
                                    switch extractNewDisj(
                                        ~ctx, 
                                        ~frmDisj=frm.frame.disj, 
                                        ~subs=frm.subs, 
                                        ~maxCtxVar=frm.workVars.numOfCtxVars-1
                                    ) {
                                        | None => Continue
                                        | Some(disj) => {
                                            let numOfArgs = frm.workVars.hypIdxToExprWithWorkVars->Js.Array2.length - 1
                                            let res = {
                                                workVars: frm.workVars.vars->Js.Array2.copy,
                                                workVarTypes: frm.workVars.types->Js.Array2.copy,
                                                disj,
                                                argLabels: comb->Js.Array2.map(s => if (s == -1) {None} else {Some(statements[s].label)}),
                                                argExprs: frm.workVars.hypIdxToExprWithWorkVars->Js.Array2.filteri((_,i) => i < numOfArgs),
                                                asrtLabel: frm.frame.label,
                                                asrtExpr: switch frm.workVars.hypIdxToExprWithWorkVars[numOfArgs] {
                                                    | None => raise(MmException({msg:`frm.workVars.hypIdxToExprWithWorkVars doesn't have asrtExpr.`}))
                                                    | Some(expr) => expr
                                                },
                                            }
                                            // checkTypesInFloatingHyps(res)
                                            onMatchFound(res)
                                        }
                                    }
                                }
                            )
                        },
                    )
                }
            )->ignore
        }
    })
}