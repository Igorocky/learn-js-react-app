open Expln_React_common
open Expln_React_Mui
open Belt
let strOrNull = Option.getWithDefault(_, "")

let firstCharCode = "a"->Js_string2.charCodeAt(0)->Belt_Int.fromFloat
let lastCharCode = firstCharCode + 25
let allChars = Array.range(firstCharCode,lastCharCode)->Array.map(Js.String2.fromCharCode)->Array.concat([
  "[", "]", "{", "}", ",", "<", ".", ">", "/", "?", ";", ":", "'", "\"", "\\", "|",
  //"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", 
  "`", "~", "1", "!", "2", "@", "3", "#", "4", "$", "5", "%", "6", "^", "7", "&", "8", "*", "9", "(", "0", ")", "-", "_", "=", "+" 
])
//let allChars = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", ]
let initRemainingKeys = () => allChars->Array.copy->Array.shuffle
let getFirstRemainingKey = keys => keys[0]->Option.getWithDefault("getFirstRemainingKey: ERROR")

@react.component
let make = () => {
  let (remainingKeys, setRemainingKeys) = React.useState(_ => initRemainingKeys())
  let (question, setQuestion) = React.useState(_ => getFirstRemainingKey(remainingKeys))
  let (isMistake, setIsMistake) = React.useState(_ => false)

  let getNextQuestion = () => {
    let newRemainingKeys = if (remainingKeys->Array.size <= 1) {
      initRemainingKeys()
    } else {
      remainingKeys->Array.keepWithIndex((_,i) => i > 0)
    }
    setRemainingKeys(_ => newRemainingKeys)
    getFirstRemainingKey(newRemainingKeys)
  }

  let acceptAnswer = str => {
    if (str == question) {
      setQuestion(_ => getNextQuestion())
      setIsMistake(_ => false)
    } else {
      setIsMistake(_ => true)
    }
  }

  <Col alignItems=#center spacing=2. justifyContent=#"space-between" style=ReactDOM.Style.make(~marginTop="100px", ())>
    <Paper style=ReactDOM.Style.make(~padding="20px", ~fontFamily="Courier New", ~fontSize="40px", ~color=if isMistake {"red"} else {"black"}, ())>
      {React.string(question)}
    </Paper>
    <TextField autoFocus=true value="" onChange=evt2str(acceptAnswer) />
  </Col>
}
