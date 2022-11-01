open Expln_test
open MM_parser
open MM_context

describe("findParentheses", (.) => {
    it("finds all parantheses", (.) => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/test/resources/demo0.mm")
        let ast = parseMmFile(mmFileText)
        let ctx = loadContext(ast, ())

        //when
        let actualFoundParens = findParentheses(ctx)

        //then
        assertEq(
            ctx->ctxExprToStr(actualFoundParens),
            ["(",")","[","]","{","}"]
        )
    })
})
