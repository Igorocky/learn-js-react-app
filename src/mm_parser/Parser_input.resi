type t

let makeParserInput: string => t
let charAtRel: (t,int)=>string
let charAt: (t,int)=>string
let proceed: (t,int)=>t
let proceedTo: (t,int)=>t
let currPositionStr: t=>string
let currPositionStrI: (t,int)=>string
let currPositionStrRel: (t,int)=>string
let toAbsolute: (t,int)=>int

