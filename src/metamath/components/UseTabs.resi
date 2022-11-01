open Expln_React_common

type promise<'a> = Js.Promise.t<'a>

type tabId = string

type tab<'a> = {
    id:tabId,
    label: string,
    closable: bool,
    data: 'a
}

type tabMethods<'a> = {
    addTab: (~label:string, ~closable:bool, ~data:'a) => promise<tabId>,
    openTab: tabId => unit,
    removeTab: tabId => unit,
    tabs: array<tab<'a>>,
    renderTabs: unit => reElem
}

let useTabs: unit => tabMethods<'a>
