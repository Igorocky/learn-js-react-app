open Expln_test
open Parser_input

describe("aaa", (.) => {
    it("fffff", (.) => {
        let inp = makeParserInput("abcdef")

        assertEq(inp->charAt(2), "c")
    })
})