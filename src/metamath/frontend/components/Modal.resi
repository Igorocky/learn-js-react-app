open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

type modalId

type openModalRef = React.ref< Js.Nullable.t<(unit => reElem) => promise<modalId>> >
let openModal: (openModalRef, unit => reElem) => promise<modalId>

@react.component
let make: (~openModalRef: openModalRef) => React.element
