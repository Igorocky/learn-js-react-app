open MM_context
open Expln_React_common
open Modal

type settings
let createDefaultSettings: unit => settings
let settingsReadFromLocStor: string => option<settings>
let getParens: settings => array<string>
let getNonSyntaxTypes: settings => array<string>
let getTypeColors: settings => Belt_MapString.t<string>

@react.component
let make: (~modalRef:modalRef, ~ctx:mmContext, ~initialSettings:settings, ~onChange: settings => unit) => reElem