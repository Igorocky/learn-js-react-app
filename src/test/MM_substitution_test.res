open Expln_test
open MM_parser
open MM_context
open MM_proof_verifier
open MM_substitution

let testIterateConstParts = (~frmExprStr:string, ~exprStr:string, ~expectedConstParts:array<(int,int)>, ~expectedMatchingConstParts:array<array<(int,int)>>) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile("./src/test/resources/substitutions-test.mm")
    let ast = parseMmFile(mmFileText)
    let ctx = loadContext(ast, ())
    ctx->applySingleStmt(Axiom({label:"test", expr: ("|- " ++ frmExprStr)->Js_string2.split(" ")}))
    let frm = switch ctx->getFrame("test") {
        | Some(frm) => frm
        | None => failWithMsg("Cannot find 'test' frame.")
    }
    let frmExpr = frm.asrt->Js_array2.sliceFrom(1)
    let expr = ctx->makeExpr(exprStr->Js_string2.split(" "))

    //when
    let (actualConstParts, actualMatchingConstParts) = test_iterateConstParts(~ctx, ~frmExpr, ~expr)

    //then
    assertEq(actualConstParts, expectedConstParts)
    assertEq(actualMatchingConstParts, expectedMatchingConstParts)
}

describe("iterateConstParts", (.) => {
    it("one option", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a -> b",
            ~exprStr = "A -> B",
            ~expectedConstParts = [(1,1)],
            ~expectedMatchingConstParts = [
                [(1,1)],
            ]
        )
    })
    it("two options", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a -> b",
            ~exprStr = "A -> B -> C",
            ~expectedConstParts = [(1,1)],
            ~expectedMatchingConstParts = [
                [(1,1)],
                [(3,3)],
            ]
        )
    })
    it("three options", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a -> b -> c",
            ~exprStr = "A -> B -> C -> D",
            ~expectedConstParts = [(1,1), (3,3)],
            ~expectedMatchingConstParts = [
                [(1,1), (3,3)],
                [(1,1), (5,5)],
                [(3,3), (5,5)],
            ]
        )
    })
})