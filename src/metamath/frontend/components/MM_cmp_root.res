open Expln_React_common
open Expln_React_Mui
open MM_context
open MM_cmp_settings
open MM_frontend
open Modal

type tabData =
    | Settings
    | Editor
    | Search
    | ProofExplorer({label:string})

@react.component
let make = () => {
    let (rootCtx, setRootCtx) = React.useState(_ => createEmptyContext())
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId} = UseTabs.useTabs()

    let onContextWasUpdated = newCtx => {
        setRootCtx(_ => newCtx)
        tabs->Js.Array2.forEach(tab => {
            switch tab.data {
                | ProofExplorer(_) => removeTab(tab.id)
                | _ => ()
            }
        })
    }

    React.useEffect0(()=>{
        updateTabs(st => {
            if (st->UseTabs.getTabs->Js_array2.length == 0) {
                let (st, _) = st->UseTabs.addTab(~label="Settings", ~closable=false, ~data=Settings)
                let (st, _) = st->UseTabs.addTab(~label="Editor", ~closable=false, ~data=Editor)
                let (st, _) = st->UseTabs.addTab(~label="Search", ~closable=false, ~data=Search)
                st
            } else {
                st
            }
        })
        None
    })

    let rndTabContent = (tab:UseTabs.tab<'a>) => {
        <div key=tab.id style=ReactDOM.Style.make(~display=if (tab.id == activeTabId) {"block"} else {"none"}, ())>
            {
                switch tab.data {
                    | Settings => <MM_cmp_settings initialSettings=createDefaultSettings() ctx=rootCtx onChange={_ => ()}/>
                    | Editor => <MM_cmp_click_counter title="Editor" />
                    | Search => <MM_cmp_click_counter title="Search" />
                    | ProofExplorer({label}) => <MM_cmp_click_counter title=label />
                }
            }
        </div>
    }

    let modalRef = useModalRef()

    <Col>
        <MM_cmp_context_selector onChange=onContextWasUpdated modalRef />
        {renderTabs()}
        {React.array(tabs->Js_array2.map(rndTabContent))}
        <Modal modalRef />
    </Col>
}