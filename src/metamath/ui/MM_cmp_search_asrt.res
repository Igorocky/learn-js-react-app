open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open MM_asrt_apply
open MM_wrk_ctx
open MM_wrk_editor
open MM_wrk_search_asrt
open MM_context
open MM_substitution
open MM_parser
open Modal

type resultForRender = React.element

type state = {
    allTypes: array<int>,
    typ: int,
    patternStr: string,
    patternErr: option<string>,
    results: option<array<applyAssertionResult>>,
    resultsForRender: option<array<resultForRender>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultsIdx: array<int>
}

let makeInitialState = (frms) => {
    if (frms->Belt_MapString.size == 0) {
        raise(MmException({msg:`Cannot search assertions when frms are empty.`}))
    }
    let allTypes = []
    frms->Belt_MapString.forEach((_,frm) => {
        let typ = frm.frame.asrt[0]
        if (!(allTypes->Js_array2.includes(typ))) {
            allTypes->Js_array2.push(typ)->ignore
        }
    })
    {
        allTypes,
        typ: allTypes[0],
        patternStr: "",
        patternErr: None,
        results: None,
        resultsForRender: None,
        resultsPerPage:10,
        resultsMaxPage:1,
        resultsPage:1,
        checkedResultsIdx: [],
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

let setType = (st,typ):state => {
    {
        ...st,
        typ
    }
}

let setPatternStr = (st,patternStr):state => {
    {
        ...st,
        patternStr
    }
}

let setPatternErr = (st,patternErr):state => {
    {
        ...st,
        patternErr
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
    let (state, setState) = React.useState(() => makeInitialState(frms))

    let actResultsRetrieved = results => {
        setState(setResults(_, results, wrkCtx, frms))
    }

    let actSearch = () => {
        let incorrectSymbol = state.patternStr->getSpaceSeparatedValuesAsArray->Js_array2.find(sym => !(wrkCtx->isConst(sym)))
        switch incorrectSymbol {
            | Some(sym) => setState(setPatternErr(_, Some(`'${sym}' - is not a constant.`)))
            | None => {
                setState(setPatternErr(_, None))
                openModal(modalRef, () => rndProgress(~text="Searching", ~pct=0.))->promiseMap(modalId => {
                    searchAssertions(
                        ~preCtxVer,
                        ~preCtx,
                        ~parenStr,
                        ~varsText,
                        ~disjText,
                        ~hyps,
                        ~typ=state.typ,
                        ~pattern=wrkCtx->ctxStrToIntsExn(state.patternStr),
                        ~onProgress = pct => updateModal(modalRef, modalId, () => rndProgress(~text="Searching", ~pct))
                    )->promiseMap(found => {
                        closeModal(modalRef, modalId)
                        actResultsRetrieved(found)
                    })
                })->ignore
            }
        }
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

    let actTypeChange = newTypeStr => {
        setState(setType(_,wrkCtx->ctxSymToIntExn(newTypeStr)))
    }

    let actPatternChange = newPatternStr => {
        setState(setPatternStr(_,newPatternStr))
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }
    
    let rndPattern = () => {
        <TextField 
            label="Pattern"
            size=#small
            style=ReactDOM.Style.make(~width="300px", ())
            autoFocus=true
            value=state.patternStr
            onChange=evt2str(actPatternChange)
        />
    }
    
    let rndTyp = () => {
        <FormControl size=#small>
            <InputLabel id="asrt-type-select-label">"Type"</InputLabel>
            <Select 
                labelId="asrt-type-select-label"
                value={wrkCtx->ctxIntToSymExn(state.typ)}
                label="Type"
                onChange=evt2str(actTypeChange)
            >
                {React.array(
                    state.allTypes->Js_array2.map(typI => {
                        let typStr = wrkCtx->ctxIntToSymExn(typI)
                        <MenuItem key=typStr value=typStr>{React.string(typStr)}</MenuItem>
                    })
                )}
            </Select>
        </FormControl>
    }

    let rndFilters = () => {
        <Col>
            <Row>
                {rndTyp()}
                {rndPattern()}
                <Button onClick={_=>actSearch()} variant=#contained>
                    {React.string("Search")}
                </Button>
                <Button onClick={_=>onCanceled()}> {React.string("Cancel")} </Button>
            </Row>
            {rndError(state.patternErr)}
        </Col>
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
                            let resIdx = minI + i
                            <ListItem key={resIdx->Belt_Int.toString}>
                                <table>
                                    <tbody>
                                        <tr>
                                            <td>
                                                <Checkbox
                                                    checked={state.checkedResultsIdx->Js.Array2.includes(resIdx)}
                                                    onChange={_ => actToggleResultChecked(resIdx)}
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