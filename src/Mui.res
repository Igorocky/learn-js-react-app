
module TextField = {
  type size = [#medium | #small]
  @module("@mui/material/TextField") @react.component
  external make: (~size:size, ~value: string) => React.element = "default"
}

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
  external make: (~onClick: option<_ => ()>=?, ~children: React.element) => React.element = "default"
}

module ListItemText = {
  @module("@mui/material/ListItemText") @react.component
  external make: (~children: React.element) => React.element = "default"
}
