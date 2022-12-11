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
    ~frm:frmSubsData,
    ~hypIdx:int,
    ~stmt:expr,
    ~hyp:expr,
    ~parenCnt:parenCnt,
):bool => {
    if (hyp[0] != stmt[0]) {
        false
    } else {
        let res = ref(false)
        iterateSubstitutions(
            ~frmExpr = hyp,
            ~expr = stmt,
            ~frmConstParts = frm.frmConstParts[hypIdx], 
            ~constParts = frm.constParts[hypIdx], 
            ~varGroups = frm.varGroups[hypIdx],
            ~subs = frm.subs,
            ~parenCnt,
            ~consumer = _ => {
                res.contents = true
                Stop
            }
        )->ignore
        res.contents
    }
}

let iterateSubstitutionsWithWorkVars = (
    ~workVars:workVars,
    ~frm:frmSubsData,
    ~hypIdx: int,
    ~continue: () => contunieInstruction
):contunieInstruction => {
    let initialNumOfWorkVars = workVars.newVars->Js_array2.length
    let predefinedSubs = frm.subs.isDefined->Js_array2.copy

    let nextVar = ref(workVars.maxVar + 1 + workVars.newVars->Js_array2.length)
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
        let newVar = newVars[i]
        let newVarType = newVarTypes[i]

        frm.subs.exprs[frmVar] = [newVar]
        frm.subs.begins[frmVar] = 0
        frm.subs.ends[frmVar] = 0
        frm.subs.isDefined[frmVar] = true

        workVars.newVars->Js_array2.push(newVar)->ignore
        workVars.newVarTypes->Js_array2.push(newVarType)->ignore
    }
    workVars.hypIdxToExprWithWorkVars[hypIdx] = Some(newExprWithWorkVars)

    let res = continue()

    predefinedSubs->Js_array2.forEachi((predefined,i) => frm.subs.isDefined[i]=predefined)
    workVars.newVars->Js_array2.removeFromInPlace(~pos=initialNumOfWorkVars)->ignore
    workVars.newVarTypes->Js_array2.removeFromInPlace(~pos=initialNumOfWorkVars)->ignore
    workVars.hypIdxToExprWithWorkVars[hypIdx] = None

    res
}

let rec iterateSubstitutionsForHyps = (
    ~workVars:workVars,
    ~frm:frmSubsData,
    ~parenCnt:parenCnt,
    ~statements:array<labeledExpr>,
    ~comb:array<int>,
    ~hypIdx:int,
    ~onMatchFound: () => contunieInstruction
):contunieInstruction => {
    if (hypIdx == comb->Js.Array2.length) {
        iterateSubstitutionsWithWorkVars(
            ~workVars,
            ~frm,
            ~hypIdx,
            ~continue = onMatchFound
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
            ~consumer = _ => {
                iterateSubstitutionsForHyps(
                    ~workVars,
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
            ~workVars,
            ~frm,
            ~hypIdx,
            ~continue = () => {
                iterateSubstitutionsForHyps(
                    ~workVars,
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

let extractNewDisj = (
    ~frmDisj:Belt_MapInt.t<Belt_SetInt.t>, 
    ~subs:subs, 
    ~maxCtxVar:int, 
    ~isDisjInCtx:(int,int)=>bool
):option<disjMutable> => {
    let result = disjMutableMake()
    let disjIsValid = verifyDisjoints(~frmDisj, ~subs, ~isDisjInCtx = (n,m) => {
        if (n <= maxCtxVar && m <= maxCtxVar) {
            isDisjInCtx(n,m)
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
    ~frm:frmSubsData,
    ~result:option<expr>,
    ~parenCnt:parenCnt,
    ~consumer:subs=>contunieInstruction,
):contunieInstruction => {
    switch result {
        | None => consumer(frm.subs)
        | Some(expr) => {
            iterateSubstitutions(
                ~frmExpr = frm.frame.asrt,
                ~expr,
                ~frmConstParts = frm.frmConstParts[frm.numOfHypsE], 
                ~constParts = frm.constParts[frm.numOfHypsE], 
                ~varGroups = frm.varGroups[frm.numOfHypsE],
                ~subs = frm.subs,
                ~parenCnt,
                ~consumer
            )
        }
    }
}

let applyAssertions = (
    ~maxVar:int,
    ~frms:array<frmSubsData>,
//    ~frmsSyntax:array<frmSubsData>,
    ~nonSyntaxTypes:array<int>,
    ~isDisjInCtx:(int,int)=>bool,
    ~statements:array<labeledExpr>,
    ~result:option<expr>=?,
    ~parenCnt:parenCnt,
    ~frameFilter:frameReduced=>bool=_=>true,
    ~onMatchFound:applyAssertionResult=>contunieInstruction,
    ()
):unit => {
    let numOfStmts = statements->Js_array2.length
    frms->Js_array2.forEach(frm => {
        if (nonSyntaxTypes->Js_array2.includes(frm.frame.asrt[0]) && frameFilter(frm.frame)) {
            iterateSubstitutionsForResult(
                ~frm,
                ~result,
                ~parenCnt,
                ~consumer = _ => {
                    let numOfHyps = frm.hypsE->Js_array2.length
                    let workVars = {
                        maxVar,
                        newVars: [],
                        newVarTypes: [],
                        hypIdxToExprWithWorkVars: Belt_Array.make(numOfHyps+1, None)
                    }
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
                                ~workVars,
                                ~frm,
                                ~parenCnt,
                                ~statements,
                                ~comb,
                                ~hypIdx=0,
                                ~onMatchFound = () => {
                                    switch extractNewDisj(
                                        ~isDisjInCtx, 
                                        ~frmDisj=frm.frame.disj, 
                                        ~subs=frm.subs, 
                                        ~maxCtxVar=maxVar
                                    ) {
                                        | None => Continue
                                        | Some(disj) => {
                                            let res = {
                                                workVars: workVars.newVars->Js.Array2.copy,
                                                workVarTypes: workVars.newVarTypes->Js.Array2.copy,
                                                disj,
                                                argLabels: comb->Js.Array2.map(s => if (s == -1) {None} else {Some(statements[s].label)}),
                                                argExprs: workVars.hypIdxToExprWithWorkVars->Js.Array2.filteri((_,i) => i < numOfHyps),
                                                asrtLabel: frm.frame.label,
                                                asrtExpr: switch workVars.hypIdxToExprWithWorkVars[numOfHyps] {
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