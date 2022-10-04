@react.component
let make = () => {
  <Expln_React_ViewSelector
    allViews = [
      {id: "1", title: "JsonParseView", render: _ => <JsonParseView/>},
      {id: "2", title: "TestSvg", render: _ => <TestSvg/>},
      {id: "3", title: "MuiDemo", render: _ => <TextFieldDemo/>},
      {id: "4", title: "KeyTrainer", render: _ => <KeyTrainer/>},
      {id: "5", title: "MM_parser", render: _ => <MM_cmp_parser/>},
    ]
    defaultViewId="5"
  />
}
