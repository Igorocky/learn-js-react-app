open Expln_React_common
open Expln_React_Mui
open MM_asrt_apply
open MM_wrk_ctx
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
    ~wrkCtxVer:string,
    ~wrkCtx:mmContext,
    ~wrkSettings:wrkSettings,
    ~onCanceled:unit=>unit,
    ~onResultsSelected:array<applyAssertionResult>=>unit
) => {
    let (state, setState) = React.useState(makeInitialState)

    let actTypUpdated = newTyp => {
        setState(setTyp(_, newTyp))
    }

    let actSearch = () => {
        ()
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