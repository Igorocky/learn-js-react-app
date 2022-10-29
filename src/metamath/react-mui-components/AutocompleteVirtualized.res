open Expln_React_common

@module("../react-mui-components/AutocompleteVirtualized.js") @react.component
external make: (
    ~options: array<string>,
    ~onChange: option<string>=>unit,
) => reElem = "default"
