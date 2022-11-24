open MM_wrk_editor
open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui

let contToText = cont => {
    switch cont {
        | Text({text}) => text
        | Tree(syntaxTreeNode) => syntaxTreeToSymbols(syntaxTreeNode)
    }
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool) => {
    <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
}

@react.component
let make = (~cont:stmtCont, ~onContentChange:stmtCont=>unit) => {
    let (newContText, setNewContText) = React.useState(_ => cont->contToText->Js_array2.joinWith(" "))
    let (editTextMode, setEditTextMode) = React.useState(_ => false)

    let rndCont = () => {
        if (editTextMode) {
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="100%", ())
                // label="Syntax types" 
                value=newContText
                onChange=evt2str(setNewContText)
            />
        } else {
            switch cont {
                | Text({text}) => React.string(text->Js_array2.joinWith(" "))
                | Tree(syntaxTreeNode) => React.string(syntaxTreeToSymbols(syntaxTreeNode)->Js_array2.joinWith(" "))
            }
        }
    }

    rndCont()
}