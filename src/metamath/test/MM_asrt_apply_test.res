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
        )

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
        )

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
        )

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
        )

        //then
        assertEq( res, [ ] )
    })
})

let testApplyAssertions = (
    ~mmFilePath:string,
    ~stopBefore:string="",
    ~stopAfter:string="",
    ~additionalStatements:array<stmt>,
    ~statements:array<labeledExpr>,
    ~fileWithExpectedResult:string,
    ()
) => {
    let printApplyAssertionResult = (ctx, res:applyAssertionResult):string => {
        ctx->openChildContext

        let workVarHypLabels = ctx->generateLabels(~prefix="workVar", ~amount=res.workVarTypes->Js_array2.length)
        let workVarTypes = res.workVarTypes->Js_array2.map(ctx->ctxIntToStrExn)
        let workVarNames = ctx->generateWorkVarNames(res.workVarTypes)

        ctx->applySingleStmt(Var({symbols:workVarNames}))
        workVarHypLabels->Js.Array2.forEachi((label,i) => {
            ctx->applySingleStmt(Floating({label, expr:[workVarTypes[i], workVarNames[i]]}))
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
                    let newStmtLabel = ctx->generateLabels(~prefix="provable", ~amount=1)
                    let label = newStmtLabel[0]
                    let exprArrStr = res.argExprs[i]->Belt_Option.getExn->Js_array2.map(ctx->ctxIntToStrExn)
                    ctx->applySingleStmt(Provable({
                        label, 
                        expr:exprArrStr,
                        proof:Table([])
                    }))
                    args->Js_array2.push(`${label}: ${exprArrStr->Js_array2.joinWith(" ")}`)->ignore
                    argLabels->Js_array2.push(label)->ignore
                }
            }
        })
        let asrtExprStr = ctx->ctxExprToStrExn(res.asrtExpr)
        ctx->resetToParentContext

        let workVarsStr = workVarHypLabels->Js.Array2.mapi((label,i) => {
            `${label} ${workVarTypes[i]} ${workVarNames[i]}`
        })->Js_array2.joinWith("\n    ")
        let argsStr = args->Js_array2.joinWith("\n    ")
        let proofStr = `:${argLabels->Js_array2.joinWith(",")}:${res.asrtLabel}`
        `------------------------\n` ++ 
            `Work variables:\n    ${workVarsStr}\nArguments:\n    ${argsStr}\nProof:\n    ${proofStr}\n` ++
            `Result:\n    ${asrtExprStr}\n\n`
    }

    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFilePath)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ~stopBefore, ~stopAfter, ())
    additionalStatements->Js_array2.forEach(ctx->applySingleStmt)
    let frms = prepareFrameProofData(ctx)
    let parenCnt = parenCntMake(ctx->makeExprExn(["(", ")", "{", "}", "[", "]"]))

    let tmpCtx = createContext(~parent=ctx, ())
    statements->Js_array2.forEach(({label,expr}) => {
        tmpCtx->applySingleStmt(Provable({
            label, 
            expr:expr->Js_array2.map(tmpCtx->ctxIntToStrExn), 
            proof:Table([])
        }))
    })
    let actualResults:Belt_MutableMapString.t<array<string>> = Belt_MutableMapString.make()

    //when
    applyAssertions(
        ~frms,
        ~nonSyntaxTypes = ctx->makeExprExn(["|-"]),
        ~statements = [ ],
        ~parenCnt,
        // ~frameFilter=frame=>frame.label=="mp",
        ~onMatchFound = res => {
            switch actualResults->Belt_MutableMapString.get(res.asrtLabel) {
                | None => actualResults->Belt_MutableMapString.set(res.asrtLabel, [printApplyAssertionResult(tmpCtx, res)])
                | Some(arr) => arr->Js.Array2.push(printApplyAssertionResult(tmpCtx, res))->ignore
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
})