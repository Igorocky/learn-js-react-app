open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open MM_asrt_apply
open MM_wrk_ctx
open MM_wrk_editor
open MM_wrk_search_asrt
open MM_context
open MM_substitution
open Modal

type state = {
    typ: string,
    results: option<array<applyAssertionResult>>,
    checkedResultsIdx: array<int>
}

let makeInitialState = () => {
    {
        typ: "",
        results: None,
        checkedResultsIdx: [],
    }
}

let setTyp = (st,typ):state => {
    {
        ...st,
        typ:typ
    }
}

let setResults = (st,results):state => {
    {
        ...st,
        results:Some(results),
        checkedResultsIdx: [],
    }
}

@react.component
let make = (
    ~modalRef:modalRef,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~wrkCtx: mmContext,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~onCanceled:unit=>unit,
    ~onResultsSelected:array<applyAssertionResult>=>unit
) => {
    let (state, setState) = React.useState(makeInitialState)

    let actTypUpdated = newTyp => {
        setState(setTyp(_, newTyp))
    }

    let actResultsRetrieved = results => {
        setState(setResults(_, results))
    }

    let rndSearchProgressDialog = () => {
        <Paper style=ReactDOM.Style.make(~padding="5px", ())>
            {React.string("Search is in progress...")}
        </Paper>
    }

    let actSearch = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => rndSearchProgressDialog())
            searchAssertions(
                ~preCtxVer,
                ~preCtx,
                ~parenStr,
                ~varsText,
                ~disjText,
                ~hyps,
                ~typ=None, 
                ~pattern=None
            )->promiseMap(found => {
                closeModal(modalRef, modalId)
                actResultsRetrieved(found)
            })
        })->ignore
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }
    
    let rndTyp = () => {
        <TextField 
            label="Type"
            size=#small
            style=ReactDOM.Style.make(~width="100px", ())
            autoFocus=true
            value=state.typ
            onChange=evt2str(actTypUpdated)
        />
    }

    let rndResult = result => {
        switch frms->Belt_MapString.get(result.asrtLabel) {
            | None => React.string(`Cannot find assertion '${result.asrtLabel}'`)
            | Some(frm) => {
                <Paper>
                    <Col>
                        {React.array(
                            frm.hypsE->Js_array2.map(hyp => {
                                <React.Fragment key={hyp.label} >
                                    {React.string(hyp.label ++ ": " ++ wrkCtx->frmIntsToStrExn(frm.frame, hyp.expr))}
                                    <Divider/>
                                </React.Fragment>
                            })
                        )}
                        { React.string(result.asrtLabel ++ ": " ++ wrkCtx->frmIntsToStrExn(frm.frame, frm.frame.asrt)) }
                    </Col>
                </Paper>
            }
        }
    }

    let rndResults = () => {
        switch state.results {
            | None => React.null
            | Some(results) => {
                <List>
                {
                    results->Js_array2.mapi((result,i) => {
                        <ListItem key={i->Belt_Int.toString}>
                            <Row alignItems=#center>
                                <Checkbox
                                    // checked={mainCheckboxState->Belt_Option.getWithDefault(false)}
                                    // onChange={_ => actToggleMainCheckbox()}
                                />
                                {rndResult(result)}
                            </Row>
                        </ListItem>
                    })->React.array
                }
                </List>
            }
        }
    }

    let rndButtons = () => {
        <Row>
            <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
            <Button onClick={_=>actSearch()} variant=#contained>
                {React.string("Search")}
            </Button>
        </Row>
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            {rndTyp()}
            {rndResults()}
            {rndButtons()}
        </Col>
    </Paper>
}