open Expln_React_common
open Expln_React_Mui
open MM_parser
open MM_cmp_single_ctx_selector
open MM_context

type mmScope = array<mmSingleScope>

@react.component
let make = (~onChange:mmContext=>unit) => {
    let (previousScope, setPreviousScope) = useState([{
        fileName:None,
        fileText:None,
        ast:None,
        allLabels:[],
        readInstr:All
    }])
    let (scope, setScope) = useStateF(previousScope)

    <Accordion>
        <AccordionSummary>
            {React.string("Preloaded context: ")}
        </AccordionSummary>
        <AccordionDetails>
            {
                <Col>
                    {React.array(
                        scope->Js_array2.mapi((singleScope,idx) => {
                            <MM_cmp_single_ctx_selector 
                                initialScope=singleScope 
                                onChange={newScope => setScope(prev => prev->Js_array2.mapi((s,i) => if i == idx {newScope} else {s}))} 
                            />
                        })
                    )}
                </Col>
            }
        </AccordionDetails>
    </Accordion>

}