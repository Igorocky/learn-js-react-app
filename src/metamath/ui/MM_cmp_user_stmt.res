open MM_wrk_editor
open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui

let contToArrStr = cont => {
    switch cont {
        | Text({text}) => text
        | Tree(syntaxTreeNode) => syntaxTreeToSymbols(syntaxTreeNode)
    }
}

let contToStr = cont => {
    cont->contToArrStr->Js_array2.joinWith(" ")
}

let strToCont = str => {
    Text({
        text: 
            str
            ->Js_string2.splitByRe(%re("/[\s\n]/"))
            ->Js_array2.map(so => so->Belt_Option.map(s=>s->Js_string2.trim)->Belt_Option.getWithDefault(""))
            ->Js_array2.filter(s => s != ""),
        syntaxError: None
    })
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool) => {
    <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
}

type state = {
    newText: string,
    proofExpanded: bool,
}

let makeInitialState = () => {
    {
        newText: "",
        proofExpanded: false
    }
}

let setNewText = (st,text):state => {
    {
        ...st,
        newText:text
    }
}

let setProofExpanded = (st,proofExpanded):state => {
    {
        ...st,
        proofExpanded
    }
}

@react.component
let make = (
    ~stmt:userStmt, 
    ~labelEditMode:bool, ~onLabelEditRequested:unit=>unit, ~onLabelEditDone:string=>unit,
    ~typEditMode:bool, ~onTypEditRequested:unit=>unit, ~onTypEditDone:userStmtType=>unit,
    ~contEditMode:bool, ~onContEditRequested:unit=>unit, ~onContEditDone:stmtCont=>unit,
    ~proofEditMode:bool, ~onProofEditRequested:unit=>unit, ~onProofEditDone:string=>unit,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())

    React.useEffect1(() => {
        if (labelEditMode) {
            setState(setNewText(_,stmt.label))
        } else if (typEditMode) {
            setState(setNewText(_,stmt.typ :> string))
        } else if (contEditMode) {
            setState(setNewText(_,contToStr(stmt.cont)))
        } else if (proofEditMode) {
            setState(setNewText(_,stmt.proof))
        }
        None
    }, [stmt.labelEditMode, stmt.typEditMode, stmt.contEditMode, stmt.proofEditMode])

    let actToggleProofExpanded = () => {
        setState(st => setProofExpanded(st, !st.proofExpanded))
    }

    let actExpandProof = expanded => {
        setState(st => setProofExpanded(st, expanded))
    }

    let actNewTextUpdated = newText => {
        setState(setNewText(_, newText))
    }
    
    let actLabelEditDone = () => {
        onLabelEditDone(state.newText)
    }
    
    let actTypEditDone = () => {
        onTypEditDone(userStmtTypeFromStr(state.newText))
    }
    
    let actContEditDone = () => {
        onContEditDone(strToCont(state.newText))
    }
    
    let actProofEditDone = () => {
        actExpandProof(true)
        onProofEditDone(state.newText)
    }

    let ctrlEnterHnd = (kbrdEvt, clbk) => {
        if (kbrdEvt->ReactEvent.Keyboard.ctrlKey && kbrdEvt->ReactEvent.Keyboard.keyCode == 13) {
            clbk()
        }
    }

    let altLeftClickHnd = (mouseEvt:ReactEvent.Mouse.t, clbk) => {
        if (mouseEvt->ReactEvent.Mouse.button == 0 && mouseEvt->ReactEvent.Mouse.altKey) {
            clbk()
        }
    }

    let rndLabel = () => {
        if (stmt.labelEditMode) {
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="100px", ())
                autoFocus=true
                value=state.newText
                onChange=evt2str(actNewTextUpdated)
                onKeyDown=ctrlEnterHnd(_, actLabelEditDone)
            />
        } else {
            <span onClick=altLeftClickHnd(_, onLabelEditRequested) >
                {React.string(stmt.label)}
            </span>
        }
    }

    let rndCont = () => {
        if (stmt.contEditMode) {
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="600px", ())
                autoFocus=true
                multiline=true
                value=state.newText
                onChange=evt2str(actNewTextUpdated)
                onKeyDown=ctrlEnterHnd(_, actContEditDone)
            />
        } else {
            <Paper onClick=altLeftClickHnd(_, onContEditRequested) >
            {
                switch stmt.cont {
                    | Text({text}) => React.string(text->Js_array2.joinWith(" "))
                    | Tree(syntaxTreeNode) => React.string(syntaxTreeToSymbols(syntaxTreeNode)->Js_array2.joinWith(" "))
                }
            }
            </Paper>
        }
    }

    let rndTyp = () => {
        if (stmt.typEditMode) {
            <FormControl size=#small >
                <Select
                    value=""
                    onChange=evt2str(newTypStr => onTypEditDone(userStmtTypeFromStr(newTypStr)))
                >
                    <MenuItem value="e">{React.string("E")}</MenuItem>
                    <MenuItem value="a">{React.string("A")}</MenuItem>
                    <MenuItem value="p">{React.string("P")}</MenuItem>
                </Select>
            </FormControl>
        } else {
            <span onClick=altLeftClickHnd(_, onTypEditRequested) style=ReactDOM.Style.make(~fontWeight="bold", ())>
                {React.string((stmt.typ :> string)->Js_string2.toUpperCase)}
            </span>
        }
    }

    let rndProof = () => {
        if (stmt.typ == #p) {
            <Col>
                <span onClick={_ => actToggleProofExpanded()} style=ReactDOM.Style.make(~cursor="pointer", ())>
                    {React.string("Proof")}
                </span>
                {
                    if (state.proofExpanded || stmt.proofEditMode) {
                        if (stmt.proofEditMode) {
                            <TextField
                                size=#small
                                style=ReactDOM.Style.make(~width="600px", ())
                                autoFocus=true
                                multiline=true
                                value=state.newText
                                onChange=evt2str(actNewTextUpdated)
                                onKeyDown=ctrlEnterHnd(_, actProofEditDone)
                            />
                        } else {
                            <Paper variant=#outlined onClick=altLeftClickHnd(_, onProofEditRequested)>
                                <pre>
                                    {React.string(stmt.proof)}
                                </pre>
                            </Paper>
                        }
                    } else {
                        React.null
                    }
                }
            </Col>
        } else {
            React.null
        }
    }

    <Row spacing=1. style=ReactDOM.Style.make(~marginTop="5px", ())>
        {rndTyp()}
        <Col>
            {rndLabel()}
            {rndCont()}
            {rndProof()}
        </Col>
    </Row>
}