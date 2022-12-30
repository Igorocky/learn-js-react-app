open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open MM_asrt_apply
open MM_wrk_ctx
open MM_wrk_editor
open MM_wrk_search_asrt
open MM_context
open Modal

type state = {
    typ: string,
    typErr: option<string>,
    result: option<array<applyAssertionResult>>,
    checkedResultIdx: array<int>
}

let makeInitialState = () => {
    {
        typ: "",
        typErr: None,
        result: None,
        checkedResultIdx: [],
    }
}

let setTyp = (st,typ):state => {
    {
        ...st,
        typ:typ
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
    ~onCanceled:unit=>unit,
    ~onResultsSelected:array<applyAssertionResult>=>unit
) => {
    let (state, setState) = React.useState(makeInitialState)

    let actTypUpdated = newTyp => {
        setState(setTyp(_, newTyp))
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
                onResultsSelected(found)
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
        <Col>
            <TextField 
                label="Type"
                size=#small
                style=ReactDOM.Style.make(~width="100px", ())
                autoFocus=true
                value=state.typ
                onChange=evt2str(actTypUpdated)
            />
            {rndError(state.typErr)}
        </Col>
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
            {rndButtons()}
        </Col>
    </Paper>
}