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

type resultForRender = React.element

type state = {
    typ: string,
    results: option<array<applyAssertionResult>>,
    resultsForRender: option<array<resultForRender>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultsIdx: array<int>
}

let makeInitialState = () => {
    {
        typ: "",
        results: None,
        resultsForRender: None,
        resultsPerPage:10,
        resultsMaxPage:1,
        resultsPage:1,
        checkedResultsIdx: [],
    }
}

let setTyp = (st,typ):state => {
    {
        ...st,
        typ:typ
    }
}

let setResults = (st,results,ctx,frms):state => {
    let maxPage = Js.Math.ceil_int(results->Js_array2.length->Belt_Int.toFloat /. st.resultsPerPage->Belt_Int.toFloat)
    {
        ...st,
        results:Some(results),
        resultsForRender:Some(
            results->Js.Array2.map(result => {
                switch frms->Belt_MapString.get(result.asrtLabel) {
                    | None => React.string(`Cannot find assertion '${result.asrtLabel}'`)
                    | Some(frm) => {
                        <Paper>
                            <Col>
                                {React.array(
                                    frm.hypsE->Js_array2.mapi((hyp,i) => {
                                        <React.Fragment key={i->Belt_Int.toString} >
                                            {React.string(hyp.label ++ ": " ++ ctx->frmIntsToStrExn(frm.frame, hyp.expr))}
                                            <Divider/>
                                        </React.Fragment>
                                    })
                                )}
                                { React.string(result.asrtLabel ++ ": " ++ ctx->frmIntsToStrExn(frm.frame, frm.frame.asrt)) }
                            </Col>
                        </Paper>
                    }
                }
            })
        ),
        resultsMaxPage: maxPage,
        resultsPage: 1,
        checkedResultsIdx: [],
    }
}

let setPage = (st,page):state => {
    {
        ...st,
        resultsPage: Js.Math.max_int(0, Js.Math.min_int(st.resultsMaxPage, page)),
    }
}

let toggleResultChecked = (st,idx) => {
    if (st.checkedResultsIdx->Js_array2.includes(idx)) {
        {
            ...st,
            checkedResultsIdx: st.checkedResultsIdx->Js.Array2.filter(i => i != idx)
        }
    } else {
        {
            ...st,
            checkedResultsIdx: st.checkedResultsIdx->Js.Array2.concat([idx])
        }
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
        setState(setResults(_, results, wrkCtx, frms))
    }

    let actSearch = () => {
        openModal(modalRef, () => rndProgress(~text="Searching", ~pct=0.))->promiseMap(modalId => {
            searchAssertions(
                ~preCtxVer,
                ~preCtx,
                ~parenStr,
                ~varsText,
                ~disjText,
                ~hyps,
                ~typ=None, 
                ~pattern=None,
                ~onProgress = pct => updateModal(modalRef, modalId, () => rndProgress(~text="Searching", ~pct))
            )->promiseMap(found => {
                closeModal(modalRef, modalId)
                actResultsRetrieved(found)
            })
        })->ignore
    }

    let actPageChange = newPage => {
        setState(setPage(_, newPage))
    }

    let actToggleResultChecked = idx => {
        setState(toggleResultChecked(_,idx))
    }

    let actChooseSelected = () => {
        switch state.results {
            | None => ()
            | Some(results) => {
                onResultsSelected(results->Js_array2.filteri((res,i) => state.checkedResultsIdx->Js.Array2.includes(i)))
            }
        }
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

    let rndFilters = () => {
        <Row>
            {rndTyp()}
            <Button onClick={_=>actSearch()} variant=#contained>
                {React.string("Search")}
            </Button>
            <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
        </Row>
    }

    let rndPagination = totalNumOfResults => {
        if (state.resultsPerPage < totalNumOfResults) {
            <Pagination count=state.resultsMaxPage page=state.resultsPage onChange={(_,newPage) => actPageChange(newPage)} />
        } else {
            React.null
        }
    }

    let rndResultButtons = () => {
        <Row>
            <Button onClick={_=>actChooseSelected()} variant=#contained>
                {React.string("Choose selected")}
            </Button>
            <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
        </Row>
    }

    let rndResults = () => {
        switch state.resultsForRender {
            | None => React.null
            | Some(resultsForRender) => {
                let items = []
                let minI = (state.resultsPage - 1) * state.resultsPerPage
                let maxI = Js.Math.min_int(minI + state.resultsPerPage - 1, resultsForRender->Js_array2.length-1)
                for i in minI to maxI {
                    let resultForRender = resultsForRender[i]
                    items->Js.Array2.push(resultForRender)->ignore
                }
                let totalNumOfResults = resultsForRender->Js.Array2.length
                <Col>
                    {rndPagination(totalNumOfResults)}
                    <List>
                    {
                        items->Js_array2.mapi((item,i) => {
                            <ListItem key={i->Belt_Int.toString}>
                                <table>
                                    <tbody>
                                        <tr>
                                            <td>
                                                <Checkbox
                                                    checked={state.checkedResultsIdx->Js.Array2.includes(i)}
                                                    onChange={_ => actToggleResultChecked(i)}
                                                />
                                            </td>
                                            <td>
                                                item
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                                // <Row alignItems=#center>
                                    
                                    
                                // </Row>
                            </ListItem>
                        })->React.array
                    }
                    </List>
                    {rndPagination(totalNumOfResults)}
                    {rndResultButtons()}
                </Col>
            }
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            {rndFilters()}
            {rndResults()}
        </Col>
    </Paper>
}