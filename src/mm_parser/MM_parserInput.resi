type parserInput = {
    text: string,
    begin: int
}

let makeParserInput: string => parserInput
let charAtRel: (parserInput,int)=>string
let charAt: (parserInput,int)=>string
let proceed: (parserInput,int)=>parserInput
let proceedTo: (parserInput,int)=>parserInput
let currPositionStr: parserInput=>string
let currPositionStrI: (parserInput,int)=>string
let currPositionStrRel: (parserInput,int)=>string
let toAbsolute: (parserInput,int)=>int
let err: (parserInput,string) => result<_,string>