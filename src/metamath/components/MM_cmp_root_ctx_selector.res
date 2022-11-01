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
    let (expanded, setExpanded) = useStateF(_ => false)
    let (thereAreChanges, setThereAreChanges) = useState(false)
    let (scope, setScope) = useStateF(_ => [createEmptySingleScope()])
    let (loadedScope, setLoadedScope) = useState([createEmptySingleScope()])
    let (ctx, setCtx) = useState(Ok(createEmptyContext()))

    let toggleAccordion = () => {
        setExpanded(prev => !prev)
    }
    
    let closeAccordion = () => {
        setExpanded(_ => false)
    }

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

    let applyChanges = () => {
        let ctx = ref(Ok(createEmptyContext()))
        scope->Js.Array2.forEachi((ss,i) => {
            if (ctx.contents->Belt_Result.isOk) {
                switch ss.ast {
                    | Some(Ok(ast)) => {
                        try {
                            let updatedCtx = ref(loadContext(
                                ast,
                                ~initialContext=ctx.contents->Belt.Result.getExn,
                                ~stopBefore = ?(if (ss.readInstr == StopBefore) {ss.label} else {None}),
                                ~stopAfter = ?(if (ss.readInstr == StopAfter) {ss.label} else {None}),
                                ()
                            ))
                            while (getNestingLevel(updatedCtx.contents) > 0) {
                                closeChildContext(updatedCtx.contents)
                            }
                            ctx.contents = Ok(updatedCtx.contents)
                        } catch {
                            | MmException({msg}) => 
                                ctx.contents = Error(`Error loading MM context from the file '${ss.fileName->Belt.Option.getWithDefault("")}': ${msg}`)
                        }
                    }
                    | Some(Error(msg)) => {
                        ctx.contents = Error(`Error parsing file '${ss.fileName->Belt.Option.getWithDefault("")}': ${msg}`)
                    }
                    | _ => {
                        if (scope->Js.Array2.length == 1 && i == 0) {
                            ()
                        } else {
                            ctx.contents = Error(`Error loading MM context: got an empty AST.`)
                        }
                    }
                }
            }
        })
        setCtx(ctx.contents)
        setLoadedScope(scope)
        setThereAreChanges(false)
        closeAccordion()
        switch ctx.contents {
            | Ok(ctx) => onChange(ctx)
            | Error(_) => onChange(createEmptyContext())
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
                <Button variant=#contained disabled={!scopeIsEmpty && !scopeIsCorrect} onClick={_=>applyChanges()} >
                    {React.string("Apply changes")}
                </Button>
            </Row>
        } else {
            React.null
        }
    }

    let getPreloadedContextInfo = () => {
        switch ctx {
            | Error(msg) => {
                <pre style=ReactDOM.Style.make(~color="red", ())>
                    {React.string(msg)}
                </pre>
            }
            | Ok(_) => {
                if (loadedScope->Js.Array2.length == 1 && loadedScope[0].fileName->Belt_Option.isNone) {
                    React.string("No MM context is loaded.")
                } else {
                    let filesInfo = loadedScope->Js_array2.map(ss => {
                        let fileName = ss.fileName->Belt_Option.getWithDefault("")
                        let readInstr = switch ss.readInstr {
                            | All => ""
                            | StopBefore => `, stoped before '${ss.label->Belt_Option.getWithDefault("")}'`
                            | StopAfter => `, stoped after '${ss.label->Belt_Option.getWithDefault("")}'`
                        }
                        fileName ++ readInstr
                    })
                    React.string("Loaded: " ++ filesInfo->Expln_utils_common.strJoin(~sep="; ", ()))
                }
            }
        }
    }

    <Accordion expanded >
        <AccordionSummaryStyled expandIcon={<Icons2.ExpandMore/>} onClick=toggleAccordion >
            {getPreloadedContextInfo()}
        </AccordionSummaryStyled>
        <AccordionDetails>
            <Col spacing=2.>
                {rndSingleScopeSelectors()}
                {rndAddButton()}
                {rndSaveButtons()}
            </Col>
        </AccordionDetails>
    </Accordion>

}