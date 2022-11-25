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
        text: str->Js_string2.split(" ")->Js_array2.map(s => s->Js_string2.trim)->Js_array2.filter(s => s != ""),
        syntaxError: None
    })
}

let contIsEmpty = cont => {
    switch cont {
        | Text({text}) => text->Js.Array2.length == 0
        | Tree(syntaxTreeNode) => syntaxTreeIsEmpty(syntaxTreeNode)
    }
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool) => {
    <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
}

type state = {
    newContText: option<string>,
}

let makeInitialState = (cont:stmtCont) => {
    {
        newContText: if contIsEmpty(cont) {Some("")} else {None}
    }
}

let setNewContText = (st,text) => {
    {
        ...st,
        newContText:text
    }
}

@react.component
let make = (~label:string, ~cont:stmtCont, ~onContentChange:stmtCont=>unit) => {
    let (state, setState) = React.useState(_ => makeInitialState(cont))

    let actEdit = () => {
        setState(st => {
            if (st.newContText->Belt_Option.isNone) {
                setNewContText(st, Some(contToStr(cont)))
            } else {
                st
            }
        })
    }

    React.useEffect1(() => {
        if (contIsEmpty(cont)) {
            actEdit()
        }
        None
    }, [cont])

    let actContTextUpdated = newContText => {
        setState(setNewContText(_, Some(newContText)))
    }
    
    let actOnEditDone = () => {
        switch state.newContText {
            | Some(newContText) => {
                onContentChange(strToCont(newContText))
                setState(setNewContText(_, None))
            }
            | _ => ()
        }
        
    }

    let rndCont = () => {
        switch state.newContText {
            | Some(newContText) => {
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="600px", ())
                    autoFocus=true
                    multiline=true
                    label
                    value=newContText
                    onChange=evt2str(actContTextUpdated)
                    onKeyDown={evt => {
                        if (evt->ReactEvent.Keyboard.ctrlKey) {
                            let keyCode = evt->ReactEvent.Keyboard.keyCode
                            if (keyCode == 13) {
                                actOnEditDone()
                            }
                        }
                    }}
                />
            }
            | None => {
                switch cont {
                    | Text({text}) => React.string(text->Js_array2.joinWith(" "))
                    | Tree(syntaxTreeNode) => React.string(syntaxTreeToSymbols(syntaxTreeNode)->Js_array2.joinWith(" "))
                }
            }
        }
    }

    <Paper
        style=ReactDOM.Style.make(~width="100%", ())
        onClick={evt=>{
            if (evt->ReactEvent.Mouse.button == 0) {
                actEdit()
            }
        }}
    >
        {rndCont()}
    </Paper>
}