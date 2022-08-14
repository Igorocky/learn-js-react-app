@react.component
let make = () => {
  <Expln_React_ViewSelector
    allViews = [
      {id: "1", title: "JsonParseView", render: _ => <JsonParseView/>}
    ]
    defaultViewId="1"
  />
}
