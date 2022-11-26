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
    newContText: string,
}

let makeInitialState = () => {
    {
        newContText: ""
    }
}

let setNewContText = (st,text):state => {
    {
        newContText:text
    }
}

@react.component
let make = (~label:string, ~cont:stmtCont, ~editMode:bool, ~onEditRequested:unit=>unit, ~onEditDone:stmtCont=>unit) => {
    let (state, setState) = React.useState(_ => makeInitialState())

    React.useEffect1(() => {
        if (editMode) {
            setState(setNewContText(_,contToStr(cont)))
        }
        None
    }, [editMode])

    let actContTextUpdated = newContText => {
        setState(setNewContText(_, newContText))
    }
    
    let actOnEditDone = () => {
        onEditDone(strToCont(state.newContText))
    }

    let rndCont = () => {
        if (editMode) {
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="600px", ())
                autoFocus=true
                multiline=true
                label
                value=state.newContText
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
        } else {
            switch cont {
                | Text({text}) => React.string(text->Js_array2.joinWith(" "))
                | Tree(syntaxTreeNode) => React.string(syntaxTreeToSymbols(syntaxTreeNode)->Js_array2.joinWith(" "))
            }
        }
    }

    <Paper
        style=ReactDOM.Style.make(~width="100%", ())
        onClick={evt=>{
            if (evt->ReactEvent.Mouse.button == 0) {
                onEditRequested()
            }
        }}
    >
        {rndCont()}
    </Paper>
}