open Expln_React_Mui
open MM_context
open MM_cmp_settings
open Modal
open UseResizeObserver

type tabData =
    | Settings
    | Editor
    | Search
    | ProofExplorer({label:string})

type state = {
    ctx: mmContext,
    ctxV: int,
    settings: settings,
    settingsV: int,
}

let createInitialState = () => {
    ctx: createContext(()),
    ctxV: 0,
    settings: createDefaultSettings(),
    settingsV: 0,
}

let setCtx = (st,ctx) => {
    {
        ...st,
        ctx,
        ctxV: st.ctxV + 1
    }
}

let setSettings = (st,settings) => {
    {
        ...st,
        settings,
        settingsV: st.settingsV + 1
    }
}

@get external getClientHeight: Dom.element => int = "clientHeight"
@new external makeMutationObserver: (array<{..}> => unit) => {..} = "ResizeObserver"

@react.component
let make = () => {
    let modalRef = useModalRef()
    let {tabs, addTab, openTab, removeTab, renderTabs, updateTabs, activeTabId} = UseTabs.useTabs()
    let (state, setState) = React.useState(_ => createInitialState())
    let (tabContentTop, setTabContentTop) = React.useState(_ => 0)

    let headerRef = React.useRef(Js.Nullable.null)
    useClientHeightObserver(headerRef, clientHeight => setTabContentTop(_ => clientHeight))

    // React.useEffect1(() => {
    //     switch headerRef.current->Js.Nullable.toOption {
    //         | Some(domElem) => {
    //             // Js.Console.log2("domElem", domElem)
    //             let observer = makeMutationObserver(mutations => {
    //                 // Js.Console.log2("mutations", mutations)
    //                 if (mutations->Js.Array2.length != 0) {
    //                     let clientHeight = mutations[0]["target"]["clientHeight"]
    //                     Js.Console.log2("clientHeight", clientHeight)
    //                     setTabContentTop(_ => clientHeight)
    //                 }
    //             })
    //             observer["observe"](. domElem, {
    //                 // "subtree": true,
    //                 // "attributeFilter": ["clientHeight"],
    //                 "attributes": true,
    //             })->ignore
    //             Some(() => observer["disconnect"](.))
    //         }
    //         | None => None
    //     }
    // }, [headerRef.current])

    let onContextWasUpdated = newCtx => {
        setState(setCtx(_,newCtx))
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
                let (st, editorTabId) = st->UseTabs.addTab(~label="Editor", ~closable=false, ~data=Editor)
                let (st, _) = st->UseTabs.addTab(~label="Search", ~closable=false, ~data=Search)
                let st = st->UseTabs.openTab(editorTabId)
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
                    | Settings => 
                        <MM_cmp_settings 
                            modalRef
                            ctx=state.ctx 
                            initialSettings=state.settings 
                            onChange={newSettings => setState(setSettings(_,newSettings))} 
                        />
                    | Editor => 
                        <MM_cmp_editor
                            top=tabContentTop
                            modalRef
                            settings=state.settings
                            settingsV=state.settingsV
                            ctx=state.ctx
                            ctxV=state.ctxV
                        />
                    | Search => <MM_cmp_click_counter title="Search" />
                    | ProofExplorer({label}) => <MM_cmp_click_counter title=label />
                }
            }
        </div>
    }


    <>
        <Col gridRef=ReactDOM.Ref.domRef(headerRef) style=ReactDOM.Style.make(~position="sticky", ~top="0px", ())>
            <MM_cmp_context_selector onChange=onContextWasUpdated modalRef />
            {renderTabs()}
        </Col>
        <Col>
            {React.array(tabs->Js_array2.map(rndTabContent))}
            <Modal modalRef />
        </Col>
    </>
    /* <Col>
        // {rndAppBar(
        //     React.array([
        //         <MM_cmp_context_selector onChange=onContextWasUpdated modalRef />,
        //         {renderTabs()},
        //     ])
        // )}
        <AppBar2/*  ref=ReactDOM.Ref.domRef(appBarRef) */>
            <MM_cmp_context_selector onChange=onContextWasUpdated modalRef />
            {renderTabs()}
        </AppBar2>
        // <div style=ReactDOM.Style.make(~height=`${appBarHeight}px`, ())>{React.string("DIV------------")}</div>
        {React.array(tabs->Js_array2.map(rndTabContent))}
        <Modal modalRef />
    </Col> */
}