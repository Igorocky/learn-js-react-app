open MM_context
open Expln_React_common
open Expln_React_Mui
open MM_wrk_FindParens
open Modal
open Expln_utils_promise

let allColors = [
    "#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabed4",
    "#469990", "#dcbeff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#a9a9a9",
    "#000000"
]

type rec settings = {
    parens: string,
    parensIsValid: bool,
    syntaxTypes: string,
    types: array<string>,
    colors: array<string>,
}

let createDefaultSettings = () => {
    {
        parens: "( ) [ ] { }",
        parensIsValid: true,
        syntaxTypes: "wff class",
        types:  [ "wff",     "term",    "setvar",  "class"],
        colors: [ "#4363d8", "#000000", "#e6194B", "#f032e6"],
    }
}

let getSpaceSeparatedValuesAsArray = str => {
    str->Js_string2.split(" ")->Js_array2.map(Js_string2.trim)->Js_array2.filter(str => str != "")
}

let getParensAsArray = st => {
    st.parens->getSpaceSeparatedValuesAsArray
}

let getSyntaxTypesAsArray = st => {
    st.syntaxTypes->getSpaceSeparatedValuesAsArray
}

let getParens: settings => array<string> = st => st->getParensAsArray

let getSyntaxTypes: settings => array<string> = st => st->getSyntaxTypesAsArray

let getCorrectedTypesAndColors = st => {
    let correctedTypes = []
    let correctedColors = []
    st.types
        ->Js_array2.mapi((typ,i) => (typ->Js_string2.trim, st.colors[i]->Js_string2.trim))
        ->Js_array2.filter(((t,c)) => t != "" && c != "")
        ->Js_array2.forEach(((t,c)) => {
            correctedTypes->Js_array2.push(t)->ignore
            correctedColors->Js_array2.push(c)->ignore
        })
    (correctedTypes, correctedColors)
}

let getTypeColors: settings => Belt_MapString.t<string> = st => {
    let (types, colors) = getCorrectedTypesAndColors(st)
    types
        ->Js_array2.mapi((typ,i) => (typ, colors[i]))
        ->Belt_MapString.fromArray
}

let setParens = (st, str) => {
    {
        ...st,
        parens: str
    }
}

let setSyntaxTypes = (st, str) => {
    {
        ...st,
        syntaxTypes: str
    }
}

let getNotUsedColors = st => {
    let usedColors = Belt_SetString.fromArray(st.colors)
    allColors->Js_array2.filter(c => !(usedColors->Belt_SetString.has(c)))
}

let addTypeColor = st => {
    {
        ...st,
        types: st.types->Js_array2.concat([""]),
        colors: st.colors->Js_array2.concat([getNotUsedColors(st)->Belt_Array.get(0)->Belt_Option.getWithDefault("")]),
    }
}

let changeType = (st,idx,newType) => {
    {
        ...st,
        types: st.types->Js_array2.mapi((e,i) => if i == idx {newType} else {e}),
    }
}

let changeColor = (st,idx,newColor) => {
    {
        ...st,
        colors: st.colors->Js_array2.mapi((e,i) => if i == idx {newColor} else {e}),
    }
}

let correctAndValidate = st => {
    let parensArr = st->getParensAsArray
    let syntaxTypesArr = st->getSyntaxTypesAsArray
    let (types, colors) = getCorrectedTypesAndColors(st)
    {
        parens: parensArr->Js_array2.joinWith(" "),
        parensIsValid: parensArr->Js_array2.length->mod(_,2) == 0,
        syntaxTypes: syntaxTypesArr->Js_array2.joinWith(" "),
        types,
        colors,
    }
}

let isValid = st => {
    st.parensIsValid
}

let eqState = (st1, st2) => {
    getParensAsArray(st1) == getParensAsArray(st2) && 
        getSyntaxTypesAsArray(st1) == getSyntaxTypesAsArray(st2) && 
        st1.types == st2.types && st1.colors == st2.colors
}

@react.component
let make = (~initialSettings:settings, ~ctx:mmContext, ~onChange: settings => unit, ~modalRef:modalRef) => {
    let (prevState, setPrevState) = React.useState(_ => initialSettings)
    let (state, setState) = React.useState(_ => initialSettings)

    let onParensChange = newParens => {
        setState(st=>{
            let st = st->setParens(newParens)
            if (st->isValid) {
                st
            } else {
                st->correctAndValidate
            }
        })
    }

    let onSyntaxTypesChange = newSyntaxTypes => {
        setState(setSyntaxTypes(_, newSyntaxTypes))
    }

    let rndFindParensProgress = (pct) => {
        rndProgress(~text=`Searching parentheses`, ~pct)
    }

    let syncParens = () => {
        openModal(modalRef, _ => rndFindParensProgress(0.))->promiseMap(modalId => {
            MM_wrk_FindParens.beginFindParens(
                ~ctx,
                ~onProgress = pct => updateModal(modalRef, modalId, () => rndFindParensProgress(pct)),
                ~onDone = parens => {
                    onParensChange(parens)
                    closeModal(modalRef, modalId)
                }
            )
        })->ignore
    }

    let applyChanges = () => {
        let st = correctAndValidate(state)
        setState(_ => st)
        if (st->isValid) {
            setPrevState(_ => st)
            onChange(st)
        }
    }
    
    let onTypeColorAdd = () => {
        setState(_ => addTypeColor(state))
    }
    let onTypeChange = (idx,newType) => {
        setState(changeType(_,idx,newType))
    }
    let onColorChange = (idx,newColor) => {
        setState(changeColor(_,idx,newColor))
    }

    let disregardChanges = () => {
        setState(_ => prevState)
    }

    <Col spacing=3. style=ReactDOM.Style.make(~margin="30px", ())>
        <Row alignItems=#center>
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="400px", ())
                label="Parentheses" 
                value=state.parens 
                onChange=evt2str(onParensChange)
                error={!state.parensIsValid}
            />
            <IconButton onClick={_ => syncParens()}>
                <Icons2.Sync/>
            </IconButton>
        </Row>
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="400px", ())
            label="Syntax types" 
            value=state.syntaxTypes
            onChange=evt2str(onSyntaxTypesChange)
        />
        <MM_cmp_colors
            types=state.types
            colors=state.colors
            availableColors=allColors
            onAdd=onTypeColorAdd
            onTypeChange
            onColorChange
        />
        {
            if (!eqState(prevState, state)) {
                <Row spacing=3. >
                    <Button disabled={!isValid(state)} onClick={_=>applyChanges()} variant=#contained>
                        {React.string("Apply changes")}
                    </Button>
                    <Button onClick={_ => disregardChanges()}>
                        {React.string("Disregard changes")}
                    </Button>
                </Row>
            } else {
                React.null
            }
        }
    </Col>
}