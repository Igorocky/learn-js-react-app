open Expln_React_common
open Expln_React_Mui
open MM_parser
open MM_context

@react.component
let make = () => {
    Js_console.log("render")
    let (rootCtx, setRootCtx) = useState(createEmptyContext())
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs} = UseTabs.useTabs()

    React.useEffect0(()=>{
        updateTabs(st => {
            if (st->UseTabs.getTabs->Js_array2.length == 0) {
                let (st, _) = st->UseTabs.addTab(~label="Settings", ~closable=true, ~data="")
                let (st, _) = st->UseTabs.addTab(~label="Editor", ~closable=true, ~data="")
                let (st, _) = st->UseTabs.addTab(~label="Search", ~closable=true, ~data="")
                st
            } else {
                st
            }
        })
        None
    })

    <Col>
        <MM_cmp_root_ctx_selector onChange=setRootCtx />
        {renderTabs()}
    </Col>
}