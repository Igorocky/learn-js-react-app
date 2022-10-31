open Expln_React_common
open Expln_React_Mui

type tabId = string

let tabId = ref(0)
let getNewId = () => {
    tabId.contents = tabId.contents + 1
    Belt_Int.toString(tabId.contents-1)
}

type tab = {
    id:tabId,
    label: string,
    closable: bool,
    render: bool => reElem
}

type useTabs = {
    addTab: (~label:string, ~closable:bool, ~render:bool=>reElem) => tabId,
    openTab: tabId => unit,
    removeTab: tabId => unit,
    renderTabs: unit => reElem
}

let useTabs = ():useTabs => {
    let (tabs, setTabs) = useStateF([])
    let (activeTabId, setActiveTabId) = useState("")

    Js.Console.log2("tabs", tabs)
    Js.Console.log2("activeTabId", activeTabId)

    let addTab = (~label, ~closable, ~render) => {
        let newId = getNewId()
        setTabs(prev => {
            let newTabs = prev->Js_array2.concat([{id:newId, label, closable, render}])
            if (newTabs->Js_array2.length == 1) {
                setActiveTabId(newTabs[0].id)
            }
            newTabs
        })
        newId
    }

    let openTab = id => {
        if (tabs->Js_array2.some(tab => tab.id == id)) {
            setActiveTabId(id)
        }
    }

    let removeTab = id => {
        setTabs(prev => {
            let newTabs = prev->Js_array2.filter(tab => tab.id != id)
            if (newTabs->Js_array2.length == 0) {
                setActiveTabId("")
            } else if (activeTabId == id) {
                setActiveTabId(newTabs[0].id)
            }
            newTabs
        })
    }

    let renderTabs = () => {
        if (tabs->Js_array2.length == 0) {
            React.null
        } else {
            <Col>
                <Tabs key=activeTabId value=activeTabId variant=#scrollable onChange={(_,id)=>openTab(id)} >
                    {React.array(
                        tabs->Js_array2.map(tab => {
                            <Tab key=tab.id value=tab.id label={
                                if (tab.closable) {
                                    <span>
                                        {React.string(tab.label)}
                                        <IconButton component="div" onClick={evt => {
                                            ReactEvent.Synthetic.stopPropagation(evt)
                                            removeTab(tab.id)
                                        }} >
                                            <Icons.Clear />
                                        </IconButton>
                                    </span>
                                } else {
                                    React.string(tab.label)
                                }
                            }/>
                        })
                    )}
                </Tabs>
            </Col>
        }
    }

    {addTab, openTab, removeTab, renderTabs}
}