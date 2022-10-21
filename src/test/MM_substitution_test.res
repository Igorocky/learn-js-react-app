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
        | None => failMsg("Cannot find 'test' frame.")
    }
    let frmExpr = frm.asrt->Js_array2.sliceFrom(1)
    let expr = ctx->makeExpr(exprStr->Js_string2.split(" "))

    //when
    let (actualConstParts, actualMatchingConstParts) = test_iterateConstParts(~ctx, ~frmExpr, ~expr)

    //then
    assertEqMsg(actualConstParts, expectedConstParts, "expectedConstParts")
    assertEqMsg(actualMatchingConstParts, expectedMatchingConstParts, "expectedMatchingConstParts")
}

describe("iterateConstParts", (.) => {
    it("one option", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a -> b",
            ~expectedConstParts = [(1,1)],
            ~exprStr = "A -> B",
            ~expectedMatchingConstParts = [
                [(1,1)],
            ]
        )
    })
    it("two options", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a -> b",
            ~expectedConstParts = [(1,1)],
            ~exprStr = "A -> B -> C",
            ~expectedMatchingConstParts = [
                [(1,1)],
                [(3,3)],
            ]
        )
    })
    it("three options", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a -> b -> c",
            ~expectedConstParts = [(1,1), (3,3)],
            ~exprStr = "A -> B -> C -> D",
            ~expectedMatchingConstParts = [
                [(1,1), (3,3)],
                [(1,1), (5,5)],
                [(3,3), (5,5)],
            ]
        )
    })
    it("assertion begins with a constant", (.) => {
        testIterateConstParts(
            ~frmExprStr = "|- a -> b",
            ~expectedConstParts = [(0,0), (2,2)],
            ~exprStr = "|- A -> B -> C",
            ~expectedMatchingConstParts = [
                [(0,0), (2,2)],
                [(0,0), (4,4)],
            ]
        )
    })
    it("assertion begins with a constant and same expression is present inside of the statement", (.) => {
        testIterateConstParts(
            ~frmExprStr = "|- a -> b",
            ~expectedConstParts = [(0,0), (2,2)],
            ~exprStr = "|- A -> |- B -> C",
            ~expectedMatchingConstParts = [
                [(0,0), (2,2)],
                [(0,0), (5,5)],
            ]
        )
    })
    it("there_are_sequences_of_more_than_one_constant", (.) => {
        testIterateConstParts(
            ~frmExprStr = "|- ( a -> b ) -> ( a -> b )",
            ~expectedConstParts = [(0,1),(3,3),(5,7),(9,9),(11,11)],
            ~exprStr = "|- ( A -> B ) -> ( A -> B )",
            ~expectedMatchingConstParts = [
                [(0,1),(3,3),(5,7),(9,9),(11,11)]
            ]
        )
    })
    it("few_options_and_asrt_ends_with_constant", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a -> b ->",
            ~expectedConstParts = [(1,1),(3,3)],
            ~exprStr = "A -> B -> C ->",
            ~expectedMatchingConstParts = [
                [(1,1),(5,5)],
                [(3,3),(5,5)],
            ]
        )
    })
    it("var_and_const", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a ->",
            ~expectedConstParts = [(1,1)],
            ~exprStr = "A -> B -> C ->",
            ~expectedMatchingConstParts = [
                [(5,5)]
            ]
        )
    })
    it("gaps_between_some_constant_parts_are_less_than_number_of_variables_and_asrt_starts_with_constant", (.) => {
        testIterateConstParts(
            ~frmExprStr = "|- a b c -> a b d",
            ~expectedConstParts = [(0,0),(4,4)],
            ~exprStr = "|- A B C -> A B D -> C",
            ~expectedMatchingConstParts = [
                [(0,0),(4,4)]
            ]
        )
    })
    it("gaps_between_some_constant_parts_are_less_than_number_of_variables_and_asrt_starts_with_non_constant", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a b c -> a b d",
            ~expectedConstParts = [(3,3)],
            ~exprStr = "A -> B -> C D E",
            ~expectedMatchingConstParts = [
                [(3,3)]
            ]
        )
        testIterateConstParts(
            ~frmExprStr = "a b c -> a b d",
            ~expectedConstParts = [(3,3)],
            ~exprStr = "A -> B C D",
            ~expectedMatchingConstParts = [ ]
        )
        testIterateConstParts(
            ~frmExprStr = "( a b )",
            ~expectedConstParts = [(0,0),(3,3)],
            ~exprStr = "( a )",
            ~expectedMatchingConstParts = [ ]
        )
    })
    it("there_are_no_constants", (.) => {
        testIterateConstParts(
            ~frmExprStr = "a b",
            ~expectedConstParts = [],
            ~exprStr = "A -> B",
            ~expectedMatchingConstParts = [ 
                [] 
            ]
        )
    })
})