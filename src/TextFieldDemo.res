open Expln_utils_common
open Expln_React_common
open Expln_React_Mui
open Belt
open MuiDemoUtils

let rndAttr = (~name:string, ~value:option<'a>, ~renderValue:'a => string) => {
  switch value {
    | Some(v) =>
      <div style=ReactDOM.Style.make(~paddingLeft="20px", ())>
        {React.string(`${name}=${renderValue(v)}`)}
      </div>
    | None => React.null
  }
}

let rndStrAttr = (~name:string, ~value:option<string>) => {
  rndAttr(~name, ~value, ~renderValue= str =>`"${str}"`)
}

@react.component
let make = () => {
  let (value, setValue) = useState(None)
  let (label, setLabel) = useState(None)
  let (size, setSize) = useState(None)
  let sizePossibleValues = [
    (#medium, "medium", "#medium"),
    (#small, "small", "#small"),
  ]
  let (adornment, setAdornment) = useState(None)
  let adornmentPossibleValues = [
    (#start, "start", "#start"),
    (#end, "end", "#end"),
  ]
  let (multiline, setMultiline) = useState(None)
  let (minRows, setMinRows) = useState(None)
  let (maxRows, setMaxRows) = useState(None)
  let (cols, setCols) = useState(None)

  let rndTextField = () => {
    let res = <TextField 
      value={value->strOrNull} 
      label={label->strOrNull} 
      onChange=evt2Str(str => setValue(Some(str)))
      ?size
      ?multiline
      ?minRows
      ?maxRows
    />
    let res = switch adornment {
      | Some(pos) => 
        let inputAdornment = 
          <InputAdornment position=pos>
            <IconButton onClick={_ => setValue(Some(""))}>
              <Icons.Clear/>
            </IconButton>
          </InputAdornment>
        React.cloneElement(res, {
          "InputProps": switch pos {
            | #start => {"startAdornment": inputAdornment, "endAdornment": React.null}
            | #end => {"startAdornment": React.null, "endAdornment": inputAdornment}
          }
        })
      | None => res
    }
    let res = switch cols {
      | Some(cols) => 
        React.cloneElement(res, {
          "inputProps": {"cols": cols}
        })
      | _ => res
    }
    res
  }

  let rndTextFieldCode = () => {
    <div style=ReactDOM.Style.make(~padding="20px", ())>
      <div> {React.string("<TextField")} </div>
      {rndStrAttr(~name="value", ~value=value)}
      {rndStrAttr(~name="label", ~value=label)}
      {rndAttr(~name="size", ~value=size, ~renderValue=v=>valToCode(sizePossibleValues, v))}
      <div> {React.string("/>")} </div>
    </div>
  }

  <Col justifyContent=#"flex-start" spacing=2. style=ReactDOM.Style.make(~padding="10px", ())>
    <Row>
      <Paper style=ReactDOM.Style.make(~padding="50px", ()) > { rndTextField() } </Paper>
      <Paper>
        {rndTextFieldCode()}
      </Paper>
    </Row>
    {rndStringParam( ~paramName="value", ~defaultValue="value", ~value=value, ~setValue=setValue)}
    {rndStringParam( ~paramName="label", ~defaultValue="label", ~value=label, ~setValue=setLabel)}
    {rndEnumParam( ~paramName="size", ~defaultValue=#medium, ~value=size, ~setValue=setSize, ~possibleValues = sizePossibleValues)}
    {rndEnumParam( ~paramName="adornment", ~defaultValue=#end, ~value=adornment, ~setValue=setAdornment, ~possibleValues = adornmentPossibleValues)}
    {rndBoolParam( ~paramName="multiline", ~defaultValue=true, ~value=multiline, ~setValue=setMultiline)}
    {rndIntSliderParam( ~paramName="minRows", ~defaultValue=1, ~value=minRows, ~setValue=setMinRows, ~min=1, ~max=10, ~step=1)}
    {rndIntSliderParam( ~paramName="maxRows", ~defaultValue=1, ~value=maxRows, ~setValue=setMaxRows, ~min=1, ~max=20, ~step=1)}
    {rndIntSliderParam( ~paramName="cols", ~defaultValue=20, ~value=cols, ~setValue=setCols, ~min=5, ~max=50, ~step=1)}
  </Col>
}