open MM_context
open Expln_React_common

type settings
let createDefaultSettings: unit => settings
let getParens: settings => array<string>
let getSyntaxTypes: settings => array<string>
let getTypeColors: settings => Belt_MapString.t<string>

@react.component
let make: (~initialSettings:settings, ~ctx:mmContext, ~onChange: settings => unit) => reElem