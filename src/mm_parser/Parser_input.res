type t = {
    text: string,
    begin: int
}

let makeParserInput = s => {text: s, begin: 0}
let toAbsolute = (t,i)=> t.begin+i
let charAtRel = (t,i)=> Js.String2.charAt(t.text, toAbsolute(t, i)) 
let charAt = (t,i)=> Js.String2.charAt(t.text, i) 
let proceed = (t,i)=>{...t, begin: t.begin+i}
let proceedTo = (t,i)=>{...t, begin: i}
let currPositionStr = t=> {
    let lengthToShow = 20
    let ellipsis = if (t.text->Js.String2.length < t.begin+lengthToShow) {""} else {"..."}
    "'" ++ t.text->Js.String2.substrAtMost(~from=t.begin, ~length=lengthToShow) ++ ellipsis ++ "'"
}
let currPositionStrI = (t,i)=> t->proceedTo(i)->currPositionStr
let currPositionStrRel = (t,i)=> t->proceedTo( t->toAbsolute(i) )->currPositionStr