open Expln_test
open MM_parser
open MM_context
open MM_syntax_prover
open MM_parenCounter

let testCanFindProof = (~mmFile, ~exprStr) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let expr = ctx->makeExpr(exprStr->Js_string2.split(" "))
    let frameProofData = prepareFrameProofData(ctx)
    let parenCnt = parenCntMake(ctx->makeExpr(["(", ")", "{", "}", "[", "]"]))
    let proofTbl = []

    //when
    let targetIdx = findProof(~ctx, ~frameProofData, ~parenCnt, ~expr, ~proofTbl)

    //then
    assertEq(true, proofTbl[targetIdx].proof->Belt_Option.isSome)
}

describe("findProof", (.) => {
    it("finds proofs for simple wffs", (.) => {
        let demo0 = "./src/metamath/test/resources/demo0.mm"
        let setReduced = "./src/metamath/test/resources/set-reduced.mm"

        testCanFindProof(~mmFile=demo0, ~exprStr="wff t = t")
        testCanFindProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )")
    })
})