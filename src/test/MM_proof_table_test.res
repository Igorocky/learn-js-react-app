open Expln_test
open MM_parser
open MM_context
open MM_syntax_prover
open MM_proof_table

let testCreateProof = (~mmFile, ~exprStr, ~expectedProof) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let ast = parseMmFile(mmFileText)
    let ctx = loadContext(ast, ())
    let expr = ctx->makeExpr(exprStr->Js_string2.split(" "))
    let proofTable = findProof(~ctx, ~expr)

    //when
    let actualProof = switch createProof(ctx, "test", proofTable) {
        | (_, Compressed({labels, compressedProofBlock})) => {
            "( " ++ (labels->Expln_utils_common.strJoin(~sep=" ", ())) ++ " ) " ++ compressedProofBlock
        }
        | p => failMsg(`Unexpected form of proof: ${Expln_utils_common.stringify(p)}`)
    }

    //then
    assertEqMsg(actualProof, expectedProof, `testCreateProof for: ${exprStr}`)
}

describe("createProof", (.) => {
    it("finds proofs for simple wffs", (.) => {
        let demo0 = "./src/test/resources/demo0.mm"
        let setReduced = "./src/test/resources/set-reduced.mm"

        testCreateProof(~mmFile=demo0, ~exprStr="wff t = t", ~expectedProof="( weq ) AAB")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( t = t -> t = t ) )", ~expectedProof="( weq wim ) AABZDDCC")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( r = r -> ( t = t -> ( r = r -> t = t ) ) ) )", ~expectedProof="( weq wim ) AACZBBCZEFEDDDD")
        testCreateProof(~mmFile=demo0, ~exprStr="wff ( ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) -> ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) )", ~expectedProof="( weq wim ) BBCAACDZEDZFDZGD")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff ph", ~expectedProof="(  ) A")
        testCreateProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )", ~expectedProof="( wb wi wn ) ABCABDBADEDEC")

        testCreateProof(~mmFile=setReduced, ~exprStr="wff -. ( ( ( ph <-> ps ) -> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) ) -> -. ( -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) -> ( ph <-> ps ) ) )", ~expectedProof="---------")
    })
})