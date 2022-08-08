open Expln_React_Mui
open Js.Console

@react.component
let make = () => {
  let (jsonStr,setJsonStr) = React.useState(_ => "")
  log2("jsonStr", jsonStr)

  let onJsonStrChange = e => setJsonStr(_ => ReactEvent.Form.target(e)["value"])

  column(~style=style(~margin="5px", ()),~childStyle=style(~margin="5px", ()), [
    textField(~key="3", ~size=#small, ~multiline=true, ~maxRows=20, ~label="JSON", ~onChange=onJsonStrChange, ()),
    button(~text="Parse JSON", ~variant=#contained, ~onClick=_ => Js.Console.log("Click!"), ()),
  ])
}
