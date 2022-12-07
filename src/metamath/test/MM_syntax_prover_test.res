open Expln_test
open MM_parser
open MM_context
open MM_syntax_prover
open MM_parenCounter
open MM_substitution

let testCanFindProof = (~mmFile, ~exprStr) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let expr = ctx->makeExprExn(exprStr->Js_string2.split(" "))
    let frms = prepareFrmSubsData(ctx)
    let parenCnt = parenCntMake(ctx->makeExprExn(["(", ")", "{", "}", "[", "]"]))
    let tbl = []
    let hyps = ctx->getAllHyps

    //when
    let targetIdx = findProof(~frms, ~parenCnt, ~expr, ~tbl, ~hyps, ~isDisjInCtx=ctx->isDisj)

    //then
    assertEq(true, tbl[targetIdx].proof->Belt_Option.isSome)
}

describe("findProof", _ => {
    it("finds proofs for simple wffs", _ => {
        let demo0 = "./src/metamath/test/resources/demo0.mm"
        let setReduced = "./src/metamath/test/resources/set-reduced.mm"

        testCanFindProof(~mmFile=demo0, ~exprStr="wff t = t")
        testCanFindProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )")
    })
})