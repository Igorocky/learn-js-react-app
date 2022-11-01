open Expln_React_common
open Expln_React_Mui
open MM_context

type tabData =
    | Editor
    | Search
    | ProofExplorer({label:string})

@react.component
let make = () => {
    let (rootCtx, setRootCtx) = useState(createEmptyContext())
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId} = UseTabs.useTabs()

    let onContextWasUpdated = newCtx => {
        setRootCtx(newCtx)
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
                let (st, _) = st->UseTabs.addTab(~label="Editor", ~closable=false, ~data=Editor)
                let (st, _) = st->UseTabs.addTab(~label="Search", ~closable=false, ~data=Search)
                let (st, _) = st->UseTabs.addTab(~label="theorem1", ~closable=true, ~data=ProofExplorer({label:"theorem1"}))
                let (st, _) = st->UseTabs.addTab(~label="theorem2", ~closable=true, ~data=ProofExplorer({label:"theorem2"}))
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
                    | Editor => <MM_cmp_click_counter title="Editor" />
                    | Search => <MM_cmp_click_counter title="Search" />
                    | ProofExplorer({label}) => <MM_cmp_click_counter title=label />
                }
            }
        </div>
    }

    <Col>
        <MM_cmp_root_ctx_selector onChange=onContextWasUpdated />
        {renderTabs()}
        {React.array(tabs->Js_array2.map(rndTabContent))}
    </Col>
}