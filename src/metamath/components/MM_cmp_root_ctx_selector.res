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
        readInstr:All
    }
}

@react.component
let make = (~onChange:mmContext=>unit) => {
    let (previousScope, setPreviousScope) = useState([createEmptySingleScope()])
    let (scope, setScope) = useStateF(previousScope)

    let thereIsAtLeastOneValidSingleScope = scope->Js_array2.some(singleScope => {
        switch singleScope.ast {
            | Some(Ok(_)) => true
            | _ => false
        }
    })
    let renderDeleteButton = scope->Js.Array2.length > 1 || scope[0].fileName->Belt_Option.isSome


    <Accordion>
        <AccordionSummary>
            {React.string("Preloaded context: ")}
        </AccordionSummary>
        <AccordionDetails>
            {
                <Col spacing=2.>
                    {React.array(
                        scope->Js_array2.mapi((singleScope,idx) => {
                            <MM_cmp_single_ctx_selector 
                                key=singleScope.id
                                initialScope=singleScope 
                                onChange={newScope => setScope(prev => prev->Js_array2.mapi((s,i) => if i == idx {newScope} else {s}))} 
                                onDelete={() => setScope(prev => {
                                    let newScope = prev->Js_array2.filter(s => s.id != singleScope.id)
                                    if (newScope->Js_array2.length == 0) {
                                        [createEmptySingleScope()]
                                    } else {
                                        newScope
                                    }
                                })}
                                renderDeleteButton
                            />
                        })
                    )}
                    {
                        if (thereIsAtLeastOneValidSingleScope) {
                            <IconButton key="add-button" onClick={_ => setScope(prev => prev->Js_array2.concat([createEmptySingleScope()]))} >
                                <Icons2.Add/>
                            </IconButton>
                        } else {
                            React.null
                        }
                    }
                </Col>
            }
        </AccordionDetails>
    </Accordion>

}