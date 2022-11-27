open MM_wrk_editor
open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool) => {
    <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
}

type state = {
    newText: string,
}

let makeInitialState = () => {
    {
        newText: "",
    }
}

let setNewText = (st,text):state => {
    {
        newText:text
    }
}

@react.component
let make = (
    ~constsText:string, 
    ~constsEditMode:bool, ~onConstsEditRequested:unit=>unit, ~onConstsEditDone:string=>unit,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())

    React.useEffect1(() => {
        if (constsEditMode) {
            setState(setNewText(_,constsText))
        }
        None
    }, [constsEditMode])

    let actNewTextUpdated = newText => {
        setState(setNewText(_, newText))
    }
    
    let actConstsEditDone = () => {
        onConstsEditDone(state.newText)
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

    let rndConsts = () => {
        if (constsEditMode) {
            <TextField
                size=#small
                style=ReactDOM.Style.make(~width="600px", ())
                autoFocus=true
                multiline=true
                value=state.newText
                onChange=evt2str(actNewTextUpdated)
                onKeyDown=ctrlEnterHnd(_, actConstsEditDone)
            />
        } else {
            <Paper variant=#outlined onClick=altLeftClickHnd(_, onConstsEditRequested)>
                <pre>
                    {React.string(constsText)}
                </pre>
            </Paper>
        }
    }

    rndConsts()
}