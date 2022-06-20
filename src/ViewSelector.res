open Mui

type view = {
  name: string,
  render: unit => React.element,
}

let allViews: array<view> = [
  {name: "JsonParseView", render: _ => <JsonParseView />}
]

let renderDefaultView = nameOfNonExistentView => <Paper>{ React.string("View not found: " ++ nameOfNonExistentView) }</Paper>

@react.component
let make = () => {
  let url = RescriptReactRouter.useUrl()

  Js.log2("url", Js.Json.stringifyAny(url))

  let openView = viewName => RescriptReactRouter.push("/" ++ viewName)

  let renderViewListItem = view =>
    <ListItem key={view.name}>
      <ListItemButton onClick={_ => openView(view.name)}> <ListItemText> {React.string(view.name)} </ListItemText> </ListItemButton>
    </ListItem>

   let renderSelectedView = viewName =>
      allViews
      -> Belt.Array.getBy(view => view.name == viewName)
      -> Belt.Option.map(v => v.render())
      -> Belt.Option.getWithDefault(renderDefaultView(viewName))


  switch url.path {
  | list{viewName} => renderSelectedView(viewName)
  | _ => <List> {allViews->Belt.Array.map(renderViewListItem)->React.array} </List>
  }
}
