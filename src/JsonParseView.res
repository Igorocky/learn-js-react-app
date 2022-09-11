open Expln_React_Mui
open Expln_React_common
open Js.Console

@react.component
let make = () => {
  let (jsonStr,setJsonStr) = React.useState(_ => "")
  log2("jsonStr", jsonStr)

  let onJsonStrChange = evt2Str(str => setJsonStr(_ => str))

  <Col spacing=2. style=ReactDOM.Style.make(~padding="10px", ())>
    <TextField size=TextField.Small multiline=true maxRows=20 label="JSON" onChange=onJsonStrChange inputProps={"size":20} />
    <Button variant=#contained onClick={_ => Js.Console.log("Click!")}>{React.string("Parse JSON")}</Button>
  </Col>
}
