@react.component
let make = () => {
  <Expln_React_ViewSelector
    allViews = [
      {id: "1", title: "JsonParseView", render: _ => <JsonParseView/>},
      {id: "2", title: "TestSvg", render: _ => <TestSvg/>},
      {id: "3", title: "MuiDemo", render: _ => <MuiDemo/>},
    ]
    defaultViewId="3"
  />
}
