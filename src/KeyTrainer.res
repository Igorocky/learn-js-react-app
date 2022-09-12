open Expln_utils_common
open Expln_React_common
open Expln_React_Mui
open Belt
let strOrNull = Option.getWithDefault(_, "")

let firstCharCode = "a"->Js_string2.charCodeAt(0)->f2i
let lastCharCode = firstCharCode + 25
let allChars = ints(firstCharCode,lastCharCode)->Array.map(Js.String2.fromCharCode)->Array.concat([
  "[", "]", "{", "}", ",", "<", ".", ">", "/", "?", ";", ":", "'", "\"", "\\", "|",
  //"`", "~", "1", "!", "2", "@", "3", "#", "4", "$", "5", "%", "6", "^", "7", "&", "8", "*", "9", "(", "0", ")", "-", "_", "=", "+" 
])
Js.Console.log2("allChars", allChars)
let initRemainingKeys = () => allChars->Array.copy->Array.shuffle
let getFirstRemainingKey = keys => keys[0]->Option.getWithDefault("getFirstRemainingKey: ERROR")

@react.component
let make = () => {
  let (remainingKeys, setRemainingKeys) = useState(initRemainingKeys())
  let (question, setQuestion) = useState(getFirstRemainingKey(remainingKeys))
  let (isMistake, setIsMistake) = useState(false)

  let getNextQuestion = () => {
    let newRemainingKeys = if (remainingKeys->Array.size <= 1) {
      initRemainingKeys()
    } else {
      remainingKeys->Array.keepWithIndex((_,i) => i > 0)
    }
    setRemainingKeys(newRemainingKeys)
    getFirstRemainingKey(newRemainingKeys)
  }

  let acceptAnswer = str => {
    if (str == question) {
      setQuestion(getNextQuestion())
      setIsMistake(false)
    } else {
      setIsMistake(true)
    }
  }

  <Col alignItems=#center spacing=2. justifyContent=#"space-between" style=ReactDOM.Style.make(~marginTop="100px", ())>
    <Paper style=ReactDOM.Style.make(~padding="20px", ~fontSize="40px", ~color=if isMistake {"red"} else {"black"}, ())>
      {React.string(question)}
    </Paper>
    <TextField autoFocus=true value="" onChange=evt2Str(acceptAnswer) />
  </Col>
}
