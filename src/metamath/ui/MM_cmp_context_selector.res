open Expln_React_Mui
open Expln_utils_promise
open MM_parser
open MM_cmp_context_selector_single
open MM_context
open Modal
open MM_worker_client

let readInstrFromStr = str => {
    switch str {
        | "all" => #all
        | "stopBefore" => #stopBefore
        | "stopAfter" => #stopAfter
        | _ => raise(MmException({msg:`Cannot convert string '${str}' to a readInstr.`}))
    }
}

type mmSingleScope = {
    id:string,
    fileName: option<string>,
    fileText: option<string>,
    ast: option<result<mmAstNode,string>>,
    allLabels: array<string>,
    readInstr: readInstr,
    label: option<string>,
}

let createEmptySingleScope = id => {
    {
        id,
        fileName:None,
        fileText:None,
        ast:None,
        allLabels:[],
        readInstr:#all,
        label:None
    }
}

type rec mmScope = {
    nextId: int,
    expanded: bool,
    singleScopes: array<mmSingleScope>,
    prev: option<mmScope>,
    loadedContextSummary: string,
}

let setFileName = (ss, fileName) => {...ss, fileName}
let setFileText = (ss, fileText) => {...ss, fileText}
let setAst = (ss, ast) => {...ss, ast}
let setAllLabels = (ss, allLabels) => {...ss, allLabels}
let setReadInstr = (ss, readInstr) => {...ss, readInstr}
let setLabel = (ss, label) => {...ss, label}
let reset = ss => createEmptySingleScope(ss.id)

let addSingleScope = st => {
    {
        ...st,
        nextId:st.nextId+1,
        singleScopes: st.singleScopes->Belt.Array.concat([createEmptySingleScope(st.nextId->Belt_Int.toString)])
    }
}
let updateSingleScope = (st,id,update) => {...st, singleScopes:st.singleScopes->Js_array2.map(ss => if ss.id == id {update(ss)} else {ss})}
let deleteSingleScope = (st,id) => {
    let st = {
        ...st, 
        singleScopes:st.singleScopes->Belt.Array.keep(ss => ss.id != id)
    }
    if (st.singleScopes->Js_array2.length == 0) {
        addSingleScope(st)
    } else {
        st
    }
}
let setExpanded = (st,expanded) => {...st, expanded}
let setPrev = (st,prev) => {...st, prev}
let setLoadedContextSummary = (st,loadedContextSummary) => {...st, loadedContextSummary}

let getSummary = st => {
    if (st.singleScopes->Js.Array2.length == 1 && st.singleScopes[0].fileName->Belt_Option.isNone) {
        "No MM context is loaded."
    } else {
        let filesInfo = st.singleScopes->Js_array2.map(ss => {
            let fileName = ss.fileName->Belt_Option.getWithDefault("")
            let readInstr = switch ss.readInstr {
                | #all => ""
                | #stopBefore => `, stoped before '${ss.label->Belt_Option.getWithDefault("")}'`
                | #stopAfter => `, stoped after '${ss.label->Belt_Option.getWithDefault("")}'`
            }
            fileName ++ readInstr
        })
        "Loaded: " ++ filesInfo->Expln_utils_common.strJoin(~sep="; ", ())
    }
}

@react.component
let make = (~onChange:mmContext=>unit, ~modalRef:modalRef) => {
    let (state, setState) = React.useState(_ => {
        {
            nextId: 1, 
            singleScopes: [createEmptySingleScope("0")], 
            expanded: true, 
            prev: None, 
            loadedContextSummary: ""
        }
    })

    React.useEffect0(() => {
        setState(prev => prev->setLoadedContextSummary(getSummary(prev)))
        None
    })

    let rndParseMmFileProgress = (fileName, pct) => {
        <span>{`Parsing ${fileName}: ${(pct *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString}%`->React.string}</span>
    }

    let parseMmFileText = (id, nameAndTextOpt) => {
        switch nameAndTextOpt {
            | None => setState(updateSingleScope(_,id,reset))
            | Some((name,text)) => {
                openModal(modalRef, _ => rndParseMmFileProgress(name, 0.))->promiseMap(modalId => {
                    regWorkerListener(msg => {
                        switch msg {
                            | MmFileParseProgress({senderId, pct}) if senderId == modalId => {
                                updateModal(modalRef, modalId, _ => rndParseMmFileProgress(name, pct))
                                Stop
                            }
                            | MmFileParsed({senderId, parseResult}) if senderId == modalId => {
                                setState(updateSingleScope(_,id,setFileName(_,Some(name))))
                                setState(updateSingleScope(_,id,setFileText(_,Some(text))))
                                setState(updateSingleScope(_,id,setReadInstr(_,#all)))
                                setState(updateSingleScope(_,id,setLabel(_,None)))
                                switch parseResult {
                                    | Error(msg) => {
                                        setState(updateSingleScope(_,id,setAst(_, Some(Error(msg)))))
                                        setState(updateSingleScope(_,id,setAllLabels(_, [])))
                                    }
                                    | Ok((ast,allLabels)) => {
                                        setState(updateSingleScope(_,id,setAst(_,Some(Ok(ast)))))
                                        setState(updateSingleScope(_,id,setAllLabels(_, allLabels->Js_array2.sortInPlace)))
                                    }
                                }
                                closeModal(modalRef, modalId)
                                StopAndUnreg
                            }
                            | _ => Cont
                        }
                    })->ignore
                    sendToWorker(ParseMmFile({senderId:modalId, mmFileText:text}))
                })->ignore
            }
        }
    }

    let toggleAccordion = () => {
        setState(prev => prev->setExpanded(!prev.expanded))
    }
    
    let closeAccordion = () => {
        setState(setExpanded(_, false))
    }

    let rndSingleScopeSelectors = () => {
        let renderDeleteButton = state.singleScopes->Js.Array2.length > 1 || state.singleScopes[0].fileName->Belt_Option.isSome
        React.array(
            state.singleScopes->Js_array2.map(singleScope => {
                <MM_cmp_context_selector_single 
                    key=singleScope.id
                    onFileChange=parseMmFileText(singleScope.id, _)
                    parseError={
                        switch singleScope.ast {
                            | Some(Error(msg)) => Some(msg)
                            | _ => None
                        }
                    }
                    readInstr=singleScope.readInstr
                    onReadInstrChange={readInstrStr => setState(updateSingleScope(_,singleScope.id,setReadInstr(_,readInstrFromStr(readInstrStr))))}
                    label=singleScope.label
                    onLabelChange={labelOpt => setState(updateSingleScope(_,singleScope.id,setLabel(_,labelOpt)))}
                    allLabels=singleScope.allLabels
                    renderDeleteButton
                    onDelete={_=>setState(deleteSingleScope(_,singleScope.id))}
                />
            })
        )
    }

    let rndAddButton = () => {
        let thereIsAtLeastOneValidSingleScope = state.singleScopes->Js_array2.some(singleScope => {
            switch singleScope.ast {
                | Some(Ok(_)) => true
                | _ => false
            }
        })
        if (thereIsAtLeastOneValidSingleScope) {
            <IconButton key="add-button" onClick={_ => setState(addSingleScope)} >
                <Icons2.Add/>
            </IconButton>
        } else {
            React.null
        }
    }

    let applyChanges = () => {
        let ctx = ref(Ok(createEmptyContext()))
        state.singleScopes->Js.Array2.forEachi((ss,i) => {
            if (ctx.contents->Belt_Result.isOk) {
                switch ss.ast {
                    | Some(Error(msg)) => {
                        ctx.contents = Error(`Error loading MM context from the file '${ss.fileName->Belt.Option.getWithDefault("")}': ${msg}`)
                    }
                    | Some(Ok(ast)) => {
                        try {
                            let updatedCtx = loadContext(
                                ast,
                                ~initialContext=ctx.contents->Belt.Result.getExn,
                                ~stopBefore = ?(if (ss.readInstr == #stopBefore) {ss.label} else {None}),
                                ~stopAfter = ?(if (ss.readInstr == #stopAfter) {ss.label} else {None}),
                                ()
                            )
                            while (getNestingLevel(updatedCtx) > 0) {
                                closeChildContext(updatedCtx)
                            }
                            ctx.contents = Ok(updatedCtx)
                        } catch {
                            | MmException({msg}) => 
                                ctx.contents = Error(`Error loading MM context from the file '${ss.fileName->Belt.Option.getWithDefault("")}': ${msg}`)
                        }
                    }
                    | None => {
                        if (state.singleScopes->Js.Array2.length == 1 && i == 0) {
                            ()
                        } else {
                            ctx.contents = Error(`Error loading MM context: got an empty AST.`)
                        }
                    }
                }
            }
        })
        switch ctx.contents {
            | Ok(ctx) => {
                setState(setPrev(_,Some(state->setPrev(None))))
                setState(prev => prev->setLoadedContextSummary(getSummary(prev)))
                closeAccordion()
                onChange(ctx)
            }
            | Error(msg) => Js.Console.log(msg)
        }
    }

    let rndSaveButtons = () => {
        let thereAreNoChanges = switch state.prev {
            | None if state.singleScopes->Js_array2.length == 1 => state.singleScopes[0].fileName->Belt_Option.isNone
            | Some(prevState) => state.singleScopes == prevState.singleScopes
            | _ => false
        }
        if (thereAreNoChanges) {
            React.null
        } else {
            let scopeIsCorrect = state.singleScopes->Js.Array2.every(ss => {
                switch ss.ast {
                    | Some(Ok(_)) => {
                        switch ss.readInstr {
                            | #all => true
                            | #stopBefore | #stopAfter => {
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
            let scopeIsEmpty = state.singleScopes->Js.Array2.length == 1 && state.singleScopes[0].fileName->Belt_Option.isNone
            <Row>
                <Button variant=#contained disabled={!scopeIsCorrect && !scopeIsEmpty} onClick={_=>applyChanges()} >
                    {React.string("Apply changes")}
                </Button>
            </Row>
        }
    }


    <Accordion expanded=state.expanded >
        <AccordionSummaryStyled expandIcon={<Icons2.ExpandMore/>} onClick=toggleAccordion >
            {state.loadedContextSummary->React.string}
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