open Expln_test
open MM_parserInput

describe("parser_input", (.) => {
    it("works", (.) => {
        let inp = makeParserInput("fun curPositionStr(i:Int):String = this.proceedTo(i).currPositionStr()")->proceed(4)

        assertEq(inp->charAtRel(0), "c")
        assertEq(inp->charAtRel(3), "P")
        assertEq(inp->charAt(2), "n")
        
        let inp2 = inp->proceed(3)
        assertEq(inp2->charAtRel(0), "P")
        
        let inp3 = inp->proceedTo(10)
        assertEq(inp3->charAtRel(0), "i")
        assertEq(inp3->currPositionStr, "'itionStr(i:Int):Stri...'")
        assertEq(inp3->currPositionStrI(4), "'curPositionStr(i:Int...'")
        assertEq(inp3->currPositionStrRel(8), "'(i:Int):String = thi...'")
        assertEq(inp3->toAbsolute(0), 10)
    })
})