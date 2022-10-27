open Expln_test
open MM_parser
open MM_context
open MM_syntax_prover

let testCanFindProof = (~mmFile, ~exprStr) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let ast = parseMmFile(mmFileText)
    let ctx = loadContext(ast, ())
    let expr = ctx->makeExpr(exprStr->Js_string2.split(" "))

    //when
    let proofTable = findProof(~ctx, ~expr)

    //then
    assertEq(true, proofTable[0].proof->Belt_Option.isSome)
}

describe("findProof", (.) => {
    it("finds proofs for simple wffs", (.) => {
        let demo0 = "./src/test/resources/demo0.mm"
        let setReduced = "./src/test/resources/set-reduced.mm"

        testCanFindProof(~mmFile=demo0, ~exprStr="wff t = t")
        testCanFindProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )")
    })
})