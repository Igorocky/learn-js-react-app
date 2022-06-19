
module TextField = {
  type size = [#medium | #small]

  @module("@mui/material/TextField") @react.component
  external make: (~size:size) => React.element = "default"
}

@react.component
let make = () => {
   let url = RescriptReactRouter.useUrl() 

   Js.log2("url", Js.Json.stringifyAny(url)) 

   switch url.path {
   | list{"json-parse"} => <JsonParseView/>
   | _ => <TextField size=#small></TextField>
   }
}
