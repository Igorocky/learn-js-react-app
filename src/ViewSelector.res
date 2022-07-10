open Mui

type view = {
  id: int,
  title: string,
  render: unit => React.element,
}

let allViews: array<view> = [
  {id: 1, title: "JsonParseView", render: _ => <JsonParseView />}
]

let renderDefaultView = idOfNonExistentView => 
    <Paper>{ 
      React.string("View not found: " ++ (idOfNonExistentView -> Belt.Int.toString)) 
    }</Paper>

@react.component
let make = () => {
  let (selectedViewId, setSelectedViewId) = React.useState(_ => Some(1))

  let openView = view => setSelectedViewId(_ => Some(view.id))

  let renderViewListItem = view =>
    <ListItem key={view.id -> Belt.Int.toString}>
      <ListItemButton onClick={_ => openView(view)}> 
        <ListItemIcon>
          <Icons.BrightnessLow/>
        </ListItemIcon>
        <ListItemText> {React.string(view.title)} </ListItemText> 
      </ListItemButton>
    </ListItem>

   let renderViewById = viewId =>
      allViews
      -> Belt.Array.getBy(view => view.id == viewId)
      -> Belt.Option.map(view => view.render())
      -> Belt.Option.getWithDefault(renderDefaultView(viewId))


  switch selectedViewId {
  | Some(id) => renderViewById(id)
  | None => <List> {allViews->Belt.Array.map(renderViewListItem)->React.array} </List>
  }
}
