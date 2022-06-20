open Mui

type view = {
  name: string,
  render: () => React.element
}

let allViews:array<view> = [
  {name:"JsonParseView", render: _ => <JsonParseView/>}
]

@react.component
let make = () => {
   let url = RescriptReactRouter.useUrl() 

   Js.log2("url", Js.Json.stringifyAny(url)) 

  let renderViewListItem = view =>
    <ListItem >
        <ListItemButton >
            <ListItemText>{React.string(view.name)}</ListItemText>
        </ListItemButton> 
    </ListItem>

   
   switch url.path {
   | list{"json-parse"} => <JsonParseView/>
   | _ => <List>{ allViews -> Belt.Array.map(renderViewListItem) -> React.array }</List>
   }
}
