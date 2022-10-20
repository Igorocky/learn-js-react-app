open Expln_test
open MM_parser
open MM_context
open MM_proof_verifier
open MM_substitution

describe("iterateConstParts", (.) => {
    it("produces correct frame and matching const parts", (.) => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/test/resources/substitutions-test.mm")
        let ast = parseMmFile(mmFileText)
        let ctx = loadContext(ast, ())
        let frmExprStr = "a -> b"
        ctx->applySingleStmt(Axiom({label:"test", expr: ("|- " ++ frmExprStr)->Js_string2.split(" ")}))
        let frm = switch ctx->getFrame("test") {
            | Some(frm) => frm
            | None => failWithMsg("Cannot find 'test' frame.")
        }
        let frmExpr = frm.asrt->Js_array2.sliceFrom(1)
        let exprStr = "A -> B"
        let expr = ctx->makeExpr(exprStr->Js_string2.split(" "))
        let (constParts, matchingConstParts) = testIterateConstParts(~ctx, ~frmExpr, ~expr)

        Js.Console.log2("constParts", constParts)
        Js.Console.log2("matchingConstParts", matchingConstParts)

        //when

        //then
    })
})