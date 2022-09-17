open Expln_utils_common
open Expln_React_common
open Expln_React_Mui
open Belt

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

let rndBoolParam = (~paramName:string, ~defaultValue:bool, ~value:option<bool>, ~setValue:option<bool>=>unit) => {
  rndParam(
    ~paramName, ~defaultValue=defaultValue, ~value, ~setValue,
    ~rndValue = () =>
      React.null
  )
}

let rndEnumParam = (~paramName:string, ~defaultValue:'a, ~value:option<'a>, ~setValue:option<'a>=>unit, ~possibleValues:array<('a,string)>) => {
  let strToVal = str => 
    switch possibleValues->Array.getBy(((_,s)) => s==str)->Option.map(((v,_)) => v) {
      | Some(val) => val
      | None => exn(`Could not transform the string representation '${str}' of '${paramName}' parameter to actual value.`)
    }
  let valToStr = val => 
    switch possibleValues->Array.getBy(((v,_)) => v==val)->Option.map(((_,s)) => s) {
      | Some(str) => str
      | None => exn(`Could not determine string representation for the value '${stringify(val)}' of '${paramName}' parameter.`)
    }
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
            | None => valToStr(defaultValue)
            | Some(val) => valToStr(val)
          }}
          onChange=evt2Str(str => setValue(Some(strToVal(str))) )
        >
        {React.array(
          possibleValues->arrMap(((_,s)) =>
            <MenuItem key=s value=s>{React.string(s)}</MenuItem>
          )
        )}
        </Select>
      </FormControl>
    }
  )
}

let strOrNull = Option.getWithDefault(_, "")

@react.component
let make = () => {
  let (value, setValue) = useState(None)
  let (label, setLabel) = useState(None)
  let (size, setSize) = useState(None)
  let (adornment, setAdornment) = useState(None)

  <Col justifyContent=#"flex-start" alignItems=#center spacing=2. style=ReactDOM.Style.make(~padding="10px", ())>
    <Row>
      <Paper style=ReactDOM.Style.make(~padding="50px", ()) >
      {
        let res = <TextField 
          value={value->strOrNull} 
          label={label->strOrNull} 
          onChange=evt2Str(str => setValue(Some(str)))
          ?size
        />
        switch adornment {
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
      }
      </Paper>
    </Row>
    {rndStringParam( ~paramName="value", ~defaultValue="value", ~value=value, ~setValue=setValue)}
    {rndStringParam( ~paramName="label", ~defaultValue="label", ~value=label, ~setValue=setLabel)}
    {rndEnumParam( ~paramName="size", ~defaultValue=#medium, ~value=size, ~setValue=setSize, ~possibleValues = [
      (#medium, "medium"),
      (#small, "small"),
    ])}
    {rndEnumParam( ~paramName="adornment", ~defaultValue=#end, ~value=adornment, ~setValue=setAdornment, ~possibleValues = [
      (#start, "start"),
      (#end, "end"),
    ])}
  </Col>
}