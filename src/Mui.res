
module TextField = {
  type size = [#medium | #small]
  @module("@mui/material/TextField") @react.component
  external make: (
    ~value: string=?,
    ~label:string=?,
    ~size:size=?, 
    ~multiline:bool=?,
    ~maxRows:int=?,
    ~rows:int=?,
  ) => React.element = "default"
}

module Button = {
  type variant = [#text|#contained|#outlined]
  @module("@mui/material/Button") @react.component
  external make: (~onClick: _ => () =?, ~variant:variant=?, ~children: React.element) => React.element = "default"
}
let button = (~text, ~variant, ~onClick=?) => 
  <Button variant onClick=?onClick>{React.string(text)}</Button>

module Paper = {
  @module("@mui/material/Paper") @react.component
  external make: (~children: React.element) => React.element = "default"
}

module List = {
  @module("@mui/material/List") @react.component
  external make: (~children: React.element) => React.element = "default"
}

module ListItem = {
  @module("@mui/material/ListItem") @react.component
  external make: (~children: React.element) => React.element = "default"
}

module ListItemButton = {
  @module("@mui/material/ListItemButton") @react.component
  external make: (~onClick: _ => () =?, ~children: React.element) => React.element = "default"
}

module ListItemText = {
  @module("@mui/material/ListItemText") @react.component
  external make: (~children: React.element) => React.element = "default"
}

module ListItemIcon = {
  @module("@mui/material/ListItemIcon") @react.component
  external make: (~children: React.element) => React.element = "default"
}

module Grid = {
  type direction = [#column|#row]
  type justifyContent = [#"flex-start"|#"flex-end"]
  type alignItems = [#"flex-start"|#"flex-end"]
  @module("@mui/material/Grid") @react.component
  external make: (
    ~container:bool=?, 
    ~item:bool=?, 
    ~direction:direction=?,
    ~justifyContent:justifyContent=?,
    ~alignItems:alignItems=?,
    ~style:ReactDOM.Style.t=?, 
    ~children: React.element
  ) => React.element = "default"
}

let createContainer = (
  ~direction:Grid.direction,
  ~style:option<ReactDOM.Style.t>=?, 
  ~childStyle:option<ReactDOM.Style.t>=?, 
  children:array<React.element>,
) =>
  <Grid container=true direction style=?style>
    {React.array(children->Belt.Array.map(React.Children.map(_, c=><Grid item=true style=?childStyle>c</Grid>)))}
  </Grid>

let column = createContainer(~direction=#column)
let row = createContainer(~direction=#row)


module Icons = {

  module Delete = {
    @module("@mui/icons-material/Delete") @react.component
    external make: () => React.element = "default"
  }

  module BrightnessLow = {
    @module("@mui/icons-material/BrightnessLow") @react.component
    external make: () => React.element = "default"
  }
}