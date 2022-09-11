open Expln_utils_common
open Expln_React_common
open Expln_React_Mui
open Belt

@react.component
let make = () => {
  let (value, setValue) = useState(None)
  let (label, setLabel) = useState(None)
  let (size, setSize) = useState(None)


  let rndParam = (~paramName:string, ~defaultValue:'a, ~value:option<'a>, ~setValue:option<'a>=>unit, ~rndValue: ()=>reElem) => {
    <Row alignItems=#center spacing=2.>
      <Checkbox checked={Option.isSome(value)} onChange=evt2Bool(checked => setValue(if checked {Some(defaultValue)} else {None})) />
      {React.string(paramName)}
      {rndValue()}
    </Row>
  }

  let rndStringParam = (~paramName:string, ~defaultValue:string, ~value:option<string>, ~setValue:option<string>=>unit) => {
    rndParam(
      ~paramName, ~defaultValue=defaultValue, ~value, ~setValue,
      ~rndValue = () =>
        <TextField disabled={value->Option.isNone} value={value->Option.getWithDefault("")} onChange=evt2Str(setValue) />
    )
  }

  let rndEnumParam = (~paramName:string, ~defaultValue:'a, ~value:option<'a>, ~setValue:option<'a>=>unit, ~possibleValues:array<('a,string)>) => {
    let labelId = "select-label-for-" ++ paramName
    rndParam(
      ~paramName, ~defaultValue, ~value, ~setValue,
      ~rndValue = () => {
        <FormControl disabled={value->Option.isNone} >
          <InputLabel id=labelId>paramName</InputLabel>
          <Select
            labelId
            label=paramName
            value={switch value {
              | None => ""
              | Some(val) => 
                switch possibleValues->Array.getBy(((v,s)) => v==val)->Option.map(((v,s))=>s) {
                  | None => exn(`Could not determine string representation for the value '${stringify(val)}' of '${paramName}' parameter.`)
                  | Some(str) => str
                }
            }}
            onChange=evt2Str(str=> {
              switch possibleValues->Array.getBy(((v,s)) => s==str)->Option.map(((v,s)) => v) {
                | Some(val) => setValue(Some(val))
                | None => exn(`Could not transform the string representation '${str}' of '${paramName}' parameter to actual value.`)
              }
            })
          >
          {React.array(
            possibleValues->arrMap(((v,s)) =>
              <MenuItem key=s value=s>{React.string(s)}</MenuItem>
            )
          )}
          </Select>
        </FormControl>
      }
    )
  }

  <Col justifyContent=#"flex-start" alignItems=#center spacing=2. style=ReactDOM.Style.make(~padding="10px", ())>
    <Row>
      <Paper style=ReactDOM.Style.make(~padding="50px", ()) >
        <TextField 
          value={value->Option.getWithDefault("")} 
          label={label->Option.getWithDefault("")} 
          ?size
        />
      </Paper>
    </Row>
    //{rndStringParam( ~paramName="value", ~defaultValue="value", ~value=value, ~setValue=setValue)}
    //{rndStringParam( ~paramName="label", ~defaultValue="label", ~value=label, ~setValue=setLabel)}
    {rndEnumParam( ~paramName="size", ~defaultValue=#medium, ~value=size, ~setValue=setSize, ~possibleValues = [
      (#medium, "medium"),
      (#small, "small"),
    ])}
  </Col>
}
