open Expln_test
open MM_parserInput
open MM_parsers

describe("parseComment", (.) => {
    it("parses comment inside of a text", (.) => {
        //given
        let inp = makeParserInput(" abc $( this is a comment $) def ")->proceedTo(5)

        //when
        let res = parseComment(inp)

        //then
        switch res {
            | Ok(res) => {
                assertEq(res.result.text, " this is a comment ")
            }
            | Error(msg) => failS(msg)
        }
    })
    
    it("parses comment at the end of a text", (.) => {
        //given
        let inp = makeParserInput(" abc $( this is a comment $)")->proceedTo(5)

        //when
        let res = parseComment(inp)

        //then
        switch res {
            | Ok(res) => {
                assertEq(res.result.text, " this is a comment ")
            }
            | Error(msg) => failS(msg)
        }
    })

    it("fails to parse an unclosed comment", (.) => {
        //given
        let inp = makeParserInput(" abc $( this is a comment $ )")->proceedTo(5)

        //when
        let res = parseComment(inp)

        //then
        switch res {
            | Ok(res) => {
                fail()
            }
            | Error(msg) => assertEq(msg, "cannot parse comment: comment is not closed [1] at '$( this is a comment...'")
        }
    })
})