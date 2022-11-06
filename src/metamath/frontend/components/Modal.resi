open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

type modalId = string

type openModalRef = React.ref< Js.Nullable.t<(unit => reElem) => promise<modalId>> >
let openModal: (openModalRef, unit => reElem) => promise<modalId>

type updateModalRef = React.ref< Js.Nullable.t<(modalId, (unit => reElem)) => unit> >
let updateModal: (updateModalRef, modalId, unit => reElem) => unit

type closeModalRef = React.ref< Js.Nullable.t<modalId => unit> >
let closeModal: (closeModalRef, modalId) => unit

@react.component
let make: (~openModalRef:openModalRef, ~updateModalRef:updateModalRef, ~closeModalRef:closeModalRef) => React.element
