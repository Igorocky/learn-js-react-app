type tabId = string
type tab = {
    id: tabId, 
    label: string, 
    closable: bool, 
    render: bool => Expln_React_common.reElem
}
type useTabs = {
    addTab: (~label: string, ~closable: bool, ~render: bool => Expln_React_common.reElem) => tabId,
    openTab: tabId => unit,
    removeTab: tabId => unit,
    renderTabs: unit => Expln_React_common.reElem
}
let useTabs: unit => useTabs
