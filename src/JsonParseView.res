open Mui

@react.component
let make = () => {
  let (jsonStr,setJsonStr) = React.useState(_ => "")

  column(~style=ReactDOM.Style.make(~margin="5px", ()),~childStyle=ReactDOM.Style.make(~margin="5px", ()), [
    <TextField key="3" size=#small multiline=true maxRows=20 label="JSON" />,
    button(~text="Parse JSON", ~variant=#contained, ~onClick=_ => Js.Console.log("Click!"), ),
  ])
}
