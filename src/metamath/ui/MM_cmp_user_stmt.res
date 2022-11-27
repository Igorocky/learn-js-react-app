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
}

let makeInitialState = () => {
    {
        newText: ""
    }
}

let setNewText = (st,text):state => {
    {
        newText:text
    }
}

@react.component
let make = (
    ~label:string, ~labelEditMode:bool, ~onLabelEditRequested:unit=>unit, ~onLabelEditDone:string=>unit,
    ~typ:userStmtType, ~typEditMode:bool, ~onTypEditRequested:unit=>unit, ~onTypEditDone:userStmtType=>unit,
    ~cont:stmtCont, ~contEditMode:bool, ~onContEditRequested:unit=>unit, ~onContEditDone:stmtCont=>unit,
    ~proof: string, ~proofEditMode:bool, ~onProofEditRequested:unit=>unit, ~onProofEditDone:string=>unit,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())

    React.useEffect1(() => {
        if (labelEditMode) {
            setState(setNewText(_,label))
        } else if (typEditMode) {
            setState(setNewText(_,typ :> string))
        } else if (contEditMode) {
            setState(setNewText(_,contToStr(cont)))
        } else if (proofEditMode) {
            setState(setNewText(_,proof))
        }
        None
    }, [labelEditMode, typEditMode, contEditMode, proofEditMode])

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
        onProofEditDone(state.newText)
    }

    let ctrlEnterHnd = (kbrdEvt, clbk) => {
        if (kbrdEvt->ReactEvent.Keyboard.ctrlKey && kbrdEvt->ReactEvent.Keyboard.keyCode == 13) {
            clbk()
        }
    }

    let shiftLeftClickHnd = (mouseEvt, clbk) => {
        if (mouseEvt->ReactEvent.Mouse.button == 0 && mouseEvt->ReactEvent.Mouse.shiftKey) {
            clbk()
        }
    }

    let rndLabel = () => {
        if (labelEditMode) {
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="100px", ())
                autoFocus=true
                value=state.newText
                onChange=evt2str(actNewTextUpdated)
                onKeyDown=ctrlEnterHnd(_, actLabelEditDone)
            />
        } else {
            <span onClick=shiftLeftClickHnd(_, onLabelEditRequested) >
                {React.string(label)}
            </span>
        }
    }

    let rndCont = () => {
        if (contEditMode) {
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
            <Paper onClick=shiftLeftClickHnd(_, onContEditRequested) >
            {
                switch cont {
                    | Text({text}) => React.string(text->Js_array2.joinWith(" "))
                    | Tree(syntaxTreeNode) => React.string(syntaxTreeToSymbols(syntaxTreeNode)->Js_array2.joinWith(" "))
                }
            }
            </Paper>
        }
    }

    <Col>
        <Row>
            <Col>
                {rndLabel()}
                {rndCont()}
            </Col>
        </Row>
    </Col>
}