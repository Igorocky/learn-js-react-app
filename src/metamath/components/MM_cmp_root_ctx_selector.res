open Expln_React_common
open Expln_React_Mui
open MM_parser
open MM_cmp_single_ctx_selector
open MM_context

type mmScope = array<mmSingleScope>

let idGen = ref(1)
let getNewId = () => {
    idGen.contents = idGen.contents + 1
    Belt_Int.toString(idGen.contents-1)
}

let createEmptySingleScope = () => {
    {
        id:getNewId(),
        fileName:None,
        fileText:None,
        ast:None,
        allLabels:[],
        readInstr:All,
        label:None
    }
}

@react.component
let make = (~onChange:mmContext=>unit) => {
    let (thereAreChanges, setThereAreChanges) = useState(false)
    let (scope, setScope) = useStateF([createEmptySingleScope()])

    let rndSingleScopeSelectors = () => {
        let renderDeleteButton = scope->Js.Array2.length > 1 || scope[0].fileName->Belt_Option.isSome
        React.array(
            scope->Js_array2.mapi((singleScope,idx) => {
                <MM_cmp_single_ctx_selector 
                    key=singleScope.id
                    initialScope=singleScope 
                    onChange={newScope => {
                        setScope(prev => prev->Js_array2.map(s => if s.id == singleScope.id {newScope} else {s}))
                        setThereAreChanges(true)
                    }} 
                    onDelete={() => {
                        setScope(prev => {
                            let newScope = prev->Js_array2.filter(s => s.id != singleScope.id)
                            if (newScope->Js_array2.length == 0) {
                                [createEmptySingleScope()]
                            } else {
                                newScope
                            }
                        })
                        setThereAreChanges(true)
                    }}
                    renderDeleteButton
                />
            })
        )
    }

    let rndAddButton = () => {
        let thereIsAtLeastOneValidSingleScope = scope->Js_array2.some(singleScope => {
            switch singleScope.ast {
                | Some(Ok(_)) => true
                | _ => false
            }
        })
        if (thereIsAtLeastOneValidSingleScope) {
            <IconButton key="add-button" onClick={_ => setScope(prev => prev->Js_array2.concat([createEmptySingleScope()]))} >
                <Icons2.Add/>
            </IconButton>
        } else {
            React.null
        }
    }

    let rndSaveButtons = () => {
        if (thereAreChanges) {
            let scopeIsCorrect = scope->Js.Array2.every(ss => {
                switch ss.ast {
                    | Some(Ok(_)) => {
                        switch ss.readInstr {
                            | All => true
                            | StopBefore | StopAfter => {
                                switch ss.label {
                                    | Some(_) => true
                                    | None => false
                                }
                            }
                        }
                    }
                    | _ => false
                }
            })
            let scopeIsEmpty = scope->Js.Array2.length == 1 && scope[0].fileName->Belt_Option.isNone
            <Row>
                <Button variant=#contained disabled={!scopeIsEmpty && !scopeIsCorrect}>
                    {React.string("Apply changes")}
                </Button>
            </Row>
        } else {
            React.null
        }
    }

    <Accordion>
        <AccordionSummary>
            {React.string("Preloaded context: ")}
        </AccordionSummary>
        <AccordionDetails>
            <Col spacing=2.>
                {rndSingleScopeSelectors()}
                {rndAddButton()}
                {rndSaveButtons()}
            </Col>
        </AccordionDetails>
    </Accordion>

}