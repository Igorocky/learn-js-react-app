open Expln_utils_common
open Expln_React_common
open Expln_React_Mui
open Belt
open MuiDemoUtils
open TextLines

let codeTabWidth = 4

let textToElems = t => React.array(t->tlMap((idx,i,s) => 
      <div key={Int.toString(idx)++s} style=ReactDOM.Style.make(~paddingLeft= Int.toString(i*10) ++ "px", ())>
        {React.string(s)}
      </div>
))

let rndAttr = (~name:string, ~value:option<'a>, ~renderValue:'a => string): TextLines.t => {
  switch value {
    | Some(v) => tlFromStrings([`${name}=${renderValue(v)}`])
    | None => tlFromStrings([])
  }
}

let rndStrAttr = (~name:string, ~value:option<string>) => {
  rndAttr(~name, ~value, ~renderValue= str =>`"${str}"`)
}

let rndIntAttr = (~name:string, ~value:option<int>) => {
  rndAttr(~name, ~value, ~renderValue= i =>Int.toString(i))
}

let rndBoolAttr = (~name:string, ~value:option<bool>) => {
  rndAttr(~name, ~value, ~renderValue= b => if b {"true"} else {"false"})
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
  let (variant, setVariant) = useState(None)
  let variantPossibleValues = [
    (#filled, "filled", "#filled"),
    (#outlined, "outlined", "#outlined"),
    (#standard, "standard", "#standard"),
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
  let (disabled, setDisabled) = useState(None)

  let rndTextField = () => {
    let res = <TextField 
      value={value->strOrNull} 
      label={label->strOrNull} 
      onChange=evt2Str(str => setValue(Some(str)))
      ?size
      ?variant
      ?multiline
      ?minRows
      ?maxRows
      ?disabled
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

  let rndAdornment = (): TextLines.t => {
    switch adornment {
      | Some(pos) => 
        let attrName = 
          switch pos {
            | #start => "startAdornment"
            | #end => "endAdornment"
          }
        tlConcatAll([
          tlFromStrings([`"InputProps": {`]),
          tlFromStrings([`"${attrName}": {`])->tlShift(1*codeTabWidth),
          tlFromStrings([`<InputAdornment position=#${valToStr(adornmentPossibleValues, pos)}>`])->tlShift(2*codeTabWidth),
          tlFromStrings([`<IconButton onClick={_ => setValue(Some(""))}>`])->tlShift(3*codeTabWidth),
          tlFromStrings([`<Icons.Clear/>`])->tlShift(4*codeTabWidth),
          tlFromStrings([`</IconButton>`])->tlShift(3*codeTabWidth),
          tlFromStrings([`</InputAdornment>`])->tlShift(2*codeTabWidth),
          tlFromStrings(["}"])->tlShift(1*codeTabWidth),
          tlFromStrings(["}"]),
        ])
      | None => tlFromStrings([""])
    }
  }

  let rndTextFieldCode = (): reElem => {
    let simpleAttrs = tlConcatAll([
        rndStrAttr(~name="value", ~value=value),
        rndStrAttr(~name="label", ~value=label),
        rndAttr(~name="size", ~value=size, ~renderValue=v=>valToCode(sizePossibleValues, v)),
        rndAttr(~name="variant", ~value=variant, ~renderValue=v=>valToCode(variantPossibleValues, v)),
        rndBoolAttr(~name="multiline", ~value=multiline),
        rndIntAttr(~name="minRows", ~value=minRows),
        rndIntAttr(~name="maxRows", ~value=maxRows),
        rndBoolAttr(~name="disabled", ~value=disabled),
      ]) -> tlShift(codeTabWidth)
    let res = tlConcatAll([
        tlFromStrings(["<TextField"]),
        simpleAttrs,
        tlFromStrings(["/>"])
      ])
    let res = if (adornment->Option.isSome || cols->Option.isSome) {
      let res = tlConcatAll([
        tlFromStrings(["let textField = "]),
        res->tlShift(codeTabWidth),
      ])
      let res = switch adornment {
        | Some(_) =>
          tlConcatAll([
            res,
            tlFromStrings(["let textField = React.cloneElement(textField, {"]),
            rndAdornment()->tlShift(codeTabWidth),
            tlFromStrings(["})"]),
          ])
        | None => res
      }
      let res = switch cols {
        | Some(cols) =>
          tlConcatAll([
            res,
            tlFromStrings(["let textField = React.cloneElement(textField, {"]),
            tlFromStrings([`"inputProps": {"cols": ${Int.toString(cols)}}`])->tlShift(codeTabWidth),
            tlFromStrings(["})"]),
          ])
        | None => res
      }
      tlConcatAll([
        res,
        tlFromStrings(["textField"]),
      ])
    } else {
      res
    }
    <div style=ReactDOM.Style.make(~padding="20px", ())>
      {textToElems(res)}
    </div>
  }

  <Col justifyContent=#"flex-start" spacing=2. style=ReactDOM.Style.make(~padding="10px", ())>
    <Row spacing=1.>
      <Paper style=ReactDOM.Style.make(~padding="50px", ()) > { rndTextField() } </Paper>
      <Paper>
        {rndTextFieldCode()}
      </Paper>
    </Row>
    {rndStringParam( ~paramName="value", ~defaultValue="value", ~value=value, ~setValue=setValue)}
    {rndStringParam( ~paramName="label", ~defaultValue="label", ~value=label, ~setValue=setLabel)}
    {rndEnumParam( ~paramName="size", ~defaultValue=#medium, ~value=size, ~setValue=setSize, ~possibleValues = sizePossibleValues)}
    {rndEnumParam( ~paramName="variant", ~defaultValue=#outlined, ~value=variant, ~setValue=setVariant, ~possibleValues = variantPossibleValues)}
    {rndEnumParam( ~paramName="adornment", ~defaultValue=#end, ~value=adornment, ~setValue=setAdornment, ~possibleValues = adornmentPossibleValues)}
    {rndBoolParam( ~paramName="multiline", ~defaultValue=true, ~value=multiline, ~setValue=setMultiline)}
    {rndIntSliderParam( ~paramName="minRows", ~defaultValue=1, ~value=minRows, ~setValue=setMinRows, ~min=1, ~max=10, ~step=1)}
    {rndIntSliderParam( ~paramName="maxRows", ~defaultValue=1, ~value=maxRows, ~setValue=setMaxRows, ~min=1, ~max=20, ~step=1)}
    {rndIntSliderParam( ~paramName="cols", ~defaultValue=20, ~value=cols, ~setValue=setCols, ~min=5, ~max=50, ~step=1)}
    {rndBoolParam( ~paramName="disabled", ~defaultValue=true, ~value=disabled, ~setValue=setDisabled)}
  </Col>
}