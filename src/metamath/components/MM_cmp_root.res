open Expln_React_common
open Expln_React_Mui
open MM_parser
open MM_context

@react.component
let make = () => {
    Js_console.log("render")
    let (rootCtx, setRootCtx) = useState(createEmptyContext())
    let {addTab, openTab, tabs, renderTabs} = UseTabs.useTabs()

    React.useEffect0(()=>{
        Js_console.log("use effect 0")
        addTab(~label="Settings", ~closable=true, ~data="")->ignore
        addTab(~label="Editor", ~closable=true, ~data="")->ignore
        addTab(~label="Search", ~closable=true, ~data="")->ignore
        None
    })

    <Col>
        <MM_cmp_root_ctx_selector onChange=setRootCtx />
        {renderTabs()}
    </Col>
}