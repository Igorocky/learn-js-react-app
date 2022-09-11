open Expln_React_common
open Expln_React_Mui
open Belt

@react.component
let make = () => {
  let (value, setValue) = useState(None)
  let (label, setLabel) = useState(None)


  let rndParam = (~paramName:string, ~defaultValue:'a, ~value:option<'a>, ~setValue:option<'a>=>unit, rndValue:'a=>reElem) => {
    <Row alignItems=#center spacing=2.>
      <Checkbox checked={Option.isSome(value)} onChange=evt2Bool(checked => setValue(if checked {Some(defaultValue)} else {None})) />
      {React.string(paramName)}
      {rndValue()}
    </Row>
  }

  let rndStringParam = (~paramName:string, ~defaultValue:string, ~value:option<string>, ~setValue:option<string>=>unit) => {
    <Row alignItems=#center spacing=2.>
      <Checkbox checked={Option.isSome(value)} onChange=evt2Bool(checked => setValue(if checked {Some(defaultValue)} else {None})) />
      {React.string(paramName)}
      <TextField disabled={value->Option.isNone} value={value->Option.getWithDefault("")} onChange=evt2Str(setValue) />
    </Row>
  }

  <Col justifyContent=#"flex-start" alignItems=#center spacing=2. style=ReactDOM.Style.make(~padding="10px", ())>
    <Row>
      <Paper style=ReactDOM.Style.make(~padding="50px", ()) >
        <TextField 
          value={value->Option.getWithDefault("")} 
          label={label->Option.getWithDefault("")} 
        />
      </Paper>
    </Row>
    {rndStringParam( ~paramName="value", ~defaultValue="value", ~value=value, ~setValue=setValue)}
    {rndStringParam( ~paramName="label", ~defaultValue="label", ~value=label, ~setValue=setLabel)}
  </Col>
}
