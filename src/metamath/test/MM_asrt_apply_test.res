open Expln_test
open MM_parser
open MM_context
open MM_substitution
open MM_parenCounter
open MM_asrt_apply

describe("iterateCombinations", _ => {
    it("iterates all possible combinations", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,_)=>true,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            }
        )->ignore

        //then
        assertEq(
            res, 
            [
                "-1 -1",
                "-1 0",
                "-1 1",
                "-1 2",
                "0 -1",
                "0 0",
                "0 1",
                "0 2",
                "1 -1",
                "1 0",
                "1 1",
                "1 2",
                "2 -1",
                "2 0",
                "2 1",
                "2 2",
            ]
        )
    })
    it("iterates all possible combinations until stoped", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,_)=>true,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                if (comb[0] == 1 && comb[1] == -1) {
                    Stop
                } else {
                    Continue
                }
            }
        )->ignore

        //then
        assertEq(
            res, 
            [
                "-1 -1",
                "-1 0",
                "-1 1",
                "-1 2",
                "0 -1",
                "0 0",
                "0 1",
                "0 2",
                "1 -1",
            ]
        )
    })

    it("iterates all applicable combinations only", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (s,h) => mod( (s+h)->Js.Math.abs_int, 2) == 1,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            }
        )->ignore

        //then
        assertEq(
            res, 
            [
                "-1 0",
                "-1 2",
                "1 0",
                "1 2",
            ]
        )
    })

    it("doesn't iterate at all if there is at least one unmatched hypothesis", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,h) => h != 0,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            }
        )->ignore

        //then
        assertEq( res, [ ] )
    })
})

let testApplyAssertions = (
    ~mmFilePath:string,
    ~stopBefore:string="",
    ~stopAfter:string="",
    ~additionalStatements:array<stmt>,
    ~statements:array<(string,string)>,
    ~frameFilter:frame=>bool=_=>true,
    ~result:option<string>=?,
    ~fileWithExpectedResult:string,
    ()
) => {
    let printApplyAssertionResult = (workCtx, res:applyAssertionResult):string => {
        workCtx->openChildContext
        let maxWorkCtxVar = workCtx->getNumOfVars - 1
        let workVarHypLabels = workCtx->generateLabels(~prefix="workVar", ~amount=res.workVarTypes->Js_array2.length)
        let workVarTypes = res.workVarTypes->Js_array2.map(workCtx->ctxIntToStrExn)
        let workVarNames = workCtx->generateWorkVarNames(res.workVarTypes)
        let disjArrStr = []
        res.disj->disjForEach(disj => {
            disjArrStr->Js.Array2.push(
                "[ " ++ 
                disj->Js.Array2.map(v => {
                    if (v <= maxWorkCtxVar) {workCtx->ctxIntToStrExn(v)} else {workVarNames[v-maxWorkCtxVar-1]}
                })->Js_array2.joinWith(" ") ++ 
                " ]"
            )->ignore
        })
        let disjStr = if (disjArrStr->Js.Array2.length == 0) {""} else {"    " ++ disjArrStr->Js.Array2.joinWith("\n    ")}

        workCtx->applySingleStmt(Var({symbols:workVarNames}))
        workVarHypLabels->Js.Array2.forEachi((label,i) => {
            workCtx->applySingleStmt(Floating({label, expr:[workVarTypes[i], workVarNames[i]]}))
        })
        let args = []
        let argLabels = []
        res.argLabels->Js.Array2.forEachi((label,i) => {
            switch label {
                | Some(label) => {
                    args->Js_array2.push(`[${label}]`)->ignore
                    argLabels->Js_array2.push(label)->ignore
                }
                | None => {
                    let newStmtLabel = workCtx->generateLabels(~prefix="provable", ~amount=1)
                    let label = newStmtLabel[0]
                    let exprArrStr = res.argExprs[i]->Belt_Option.getExn->Js_array2.map(workCtx->ctxIntToStrExn)
                    workCtx->applySingleStmt(Provable({
                        label, 
                        expr:exprArrStr,
                        proof:Table([])
                    }))
                    args->Js_array2.push(`${label}: ${exprArrStr->Js_array2.joinWith(" ")}`)->ignore
                    argLabels->Js_array2.push(label)->ignore
                }
            }
        })
        let asrtExprStr = workCtx->ctxExprToStrExn(res.asrtExpr)
        workCtx->resetToParentContext

        let workVarsStr = if (workVarHypLabels->Js_array2.length == 0) {
            ""
        } else {
            "    " ++ workVarHypLabels->Js.Array2.mapi((label,i) => {
                `${label} ${workVarTypes[i]} ${workVarNames[i]}`
            })->Js_array2.joinWith("\n    ")
        }
        let argsStr = if (args->Js.Array2.length > 0) {
            "    " ++ args->Js_array2.joinWith("\n    ")
        } else {
            ""
        }
        let proofStr = `:${argLabels->Js_array2.joinWith(",")}:${res.asrtLabel}`
        `------------------------\n` ++ 
            `Work variables:\n${workVarsStr}\nDisjoints:\n${disjStr}\nArguments:\n${argsStr}\nProof:\n    ${proofStr}\n` ++
            `Result:\n    ${asrtExprStr}\n\n`
    }

    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(mmFileText, ())
    let preCtx = loadContext(ast, ~stopBefore, ~stopAfter, ())
    additionalStatements->Js_array2.forEach(preCtx->applySingleStmt)
    let workCtx = createContext(~parent=preCtx, ())
    let frms = prepareFrmSubsData(workCtx)
    let parenCnt = parenCntMake(workCtx->makeExprExn(["(", ")", "{", "}", "[", "]"]))

    let actualResults:Belt_MutableMapString.t<array<string>> = Belt_MutableMapString.make()

    //when
    applyAssertions(
        ~isDisjInCtx = workCtx->isDisj,
        ~frms,
        ~nonSyntaxTypes = preCtx->makeExprExn(["|-"]),
        ~statements = statements->Js_array2.map(((label,exprStr)) => {
            {
                label, 
                expr:exprStr->getSpaceSeparatedValuesAsArray->makeExprExn(workCtx,_)
            }
        }),
        ~parenCnt,
        ~frameFilter,
        ~result=?result->Belt_Option.map(str => str->getSpaceSeparatedValuesAsArray->makeExprExn(workCtx,_)),
        ~onMatchFound = res => {
            switch actualResults->Belt_MutableMapString.get(res.asrtLabel) {
                | None => actualResults->Belt_MutableMapString.set(res.asrtLabel, [printApplyAssertionResult(workCtx, res)])
                | Some(arr) => arr->Js.Array2.push(printApplyAssertionResult(workCtx, res))->ignore
            }
            // Js.Console.log("onMatchFound ------------------------------------------------------------------")
            // Js.Console.log(printApplyAssertionResult(res))
            Continue
        },
        ()
    )

    //then
    let actualResultsStr = actualResults->Belt_MutableMapString.keysToArray
        ->Js_array2.sortInPlace
        ->Js_array2.map(astrLabel => {
            switch actualResults->Belt_MutableMapString.get(astrLabel) {
                | None => failMsg("actualResults->Belt_MutableMapString.get(astrLabel) == None")
                | Some(arr) => arr->Js_array2.joinWith("\n")
            }
        })
        ->Js_array2.joinWith("\n")
    let expectedResultStr = Expln_utils_files.readStringFromFile(fileWithExpectedResult)
        ->Js.String2.replaceByRe(%re("/\r/g"), "")
    if (actualResultsStr != expectedResultStr) {
        let fileWithActualResult = fileWithExpectedResult ++ ".actual"
        Expln_utils_files.writeStringToFile(fileWithActualResult, actualResultsStr)
        assertEq( fileWithActualResult, fileWithExpectedResult )
    }
}

describe("applyAssertions", _ => {
    let demo0 = "./src/metamath/test/resources/demo0.mm"
    it("applies assertions when there are no statements", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [],
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-no-statements.txt",
            ()
        )
    })
    it("applies assertions when there is one statement, for modus ponens", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [
                ("p1","|- ( t + 0 ) = t")
            ],
            ~frameFilter=frame=>frame.label=="mp",
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-one-statement-mp.txt",
            ()
        )
    })
    it("applies assertions when there is one statement, for all assertions from demo0", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [
                ("p1","|- ( t + 0 ) = t")
            ],
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-one-statement.txt",
            ()
        )
    })
    it("applies assertions when there is one statement and a result, for mp assertion from demo0", _ => {
        testApplyAssertions(
            ~mmFilePath = demo0,
            ~stopAfter = "th1",
            ~additionalStatements = [],
            ~statements = [
                ("p1","|- P")
            ],
            ~frameFilter = frame => frame.label == "mp",
            ~result="|- P",
            ~fileWithExpectedResult = "./src/metamath/test/resources/applyAssertions-test-data/expected-one-statement-with-result.txt",
            ()
        )
    })
})