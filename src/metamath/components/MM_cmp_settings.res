open MM_context
open Expln_React_common
open Expln_React_Mui

type rec settings = {
    parens: string,
    parensIsValid: bool,
    typeColors: Belt_MapString.t<string>,
}

let createDefaultSettings = () => {
    {
        parens: "( ) [ ] { }",
        parensIsValid: true,
        typeColors: Belt_MapString.fromArray([ ("wff","blue"), ("term","black"), ("setvar","red"), ("class","magenta") ]),
    }
}

let getParensAsArray = st => {
    st.parens->Js_string2.split(" ")->Js_array2.map(Js_string2.trim)->Js_array2.filter(str => str != "")
}

let getParens: settings => array<string> = st => st->getParensAsArray

let getTypeColors: settings => Belt_MapString.t<string> = st => st.typeColors

let setParens = (st, str) => {
    {
        ...st,
        parens: str
    }
}

let setColor = (st, typ, color) => {
    {
        ...st,
        typeColors: st.typeColors->Belt_MapString.set(typ, color)
    }
}


let validate = st => {
    {
        ...st,
        parensIsValid: st->getParensAsArray->Js_array2.length->mod(_,2) == 0
    }
}

let isValid = st => {
    st.parensIsValid
}

let eqState = (st1, st2) => {
    getParensAsArray(st1) == getParensAsArray(st2) && st1.typeColors == st2.typeColors
}

@react.component
let make = (~initialSettings:settings, ~ctx:mmContext, ~onChange: settings => unit) => {
    let (prevState, setPrevState) = useState(initialSettings)
    let (state, setState) = useState(initialSettings)

    let onParensChange = newParens => {
        let st = ref(state->setParens(newParens))
        if (!isValid(st.contents)) {
            st.contents = validate(st.contents)
        }
        setState(st.contents)
    }

    let syncParens = () => {
        findParentheses(ctx)->ctxExprToStr(ctx, _)->Expln_utils_common.strJoin(~sep=" ", ())->onParensChange
    }

    let applyChanges = () => {
        let st = validate(state)
        setState(st)
        if (st->isValid) {
            setPrevState(st)
            onChange(st)
        }
    }

    let disregardChanges = () => {
        setState(prevState)
    }

    <Col spacing=3. style=ReactDOM.Style.make(~margin="30px", ())>
        <Row alignItems=#center>
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="400px", ())
                label="Parentheses" 
                value=state.parens 
                onChange=evt2Str(onParensChange) 
                error={!state.parensIsValid}
            />
            <IconButton onClick={_ => syncParens()}>
                <Icons2.Sync/>
            </IconButton>
        </Row>
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