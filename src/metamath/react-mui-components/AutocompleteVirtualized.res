open Expln_React_common

type size = [#small | #medium]

@module("../react-mui-components/AutocompleteVirtualized.js") @react.component
external make: (
    ~options: array<string>,
    ~onChange: option<string>=>unit,
    ~size:size=?,
) => reElem = "default"
