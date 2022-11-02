open Expln_utils_common
open Expln_React_common
open Expln_React_Mui
open Belt

let rndParam = (~paramName:string, ~defaultValue:'a, ~value:option<'a>, ~setValue:option<'a>=>unit, ~rndValue: ()=>reElem) => {
  <Row alignItems=#center spacing=2.>
    <Checkbox checked={Option.isSome(value)} onChange=evt2bool(checked => setValue(if checked {Some(defaultValue)} else {None})) />
    {React.string(paramName)}
    {rndValue()}
  </Row>
}

let rndStringParam = (~paramName:string, ~defaultValue:string, ~value:option<string>, ~setValue:option<string>=>unit) => {
  rndParam(
    ~paramName, ~defaultValue=defaultValue, ~value, ~setValue,
    ~rndValue = () =>
      <TextField disabled={value->Option.isNone} value={value->Option.getWithDefault("")} onChange=evt2str(setValue) />
  )
}

let rndBoolParam = (~paramName:string, ~defaultValue:bool, ~value:option<bool>, ~setValue:option<bool>=>unit) => {
  rndParam(
    ~paramName, ~defaultValue=defaultValue, ~value, ~setValue,
    ~rndValue = () =>
      React.null
  )
}

let rndIntSliderParam = (~paramName:string, ~defaultValue:int, ~value:option<int>, ~setValue:option<int>=>unit, ~min:int, ~max:int, ~step:int) => {
  rndParam(
    ~paramName, ~defaultValue=defaultValue, ~value, ~setValue,
    ~rndValue = () =>
      <Box sx={"width": 300}>
        <Slider 
          disabled={value->Option.isNone} 
          size=#small 
          defaultValue=Js.Int.toFloat(defaultValue) 
          valueLabelDisplay=#auto 
          min=Js.Int.toFloat(min) 
          max=Js.Int.toFloat(max) 
          step=Js.Int.toFloat(step) 
          onChange={(_, v) => setValue(Some(Float.toInt(v)))} />
      </Box>
  )
}

let strToVal = (~paramName="unknown-parameter", possibleValues:array<('a,string,string)>, str:string):'a => 
  switch possibleValues->Array.getBy(((_,s,_)) => s==str)->Option.map(((v,_,_)) => v) {
    | Some(val) => val
    | None => exn(`Could not transform the string representation '${str}' of '${paramName}' parameter to actual value.`)
  }

let valToStr = (~paramName="unknown-parameter", possibleValues:array<('a,string,string)>, val:'a): string  => 
  switch possibleValues->Array.getBy(((v,_,_)) => v==val)->Option.map(((_,s,_)) => s) {
    | Some(str) => str
    | None => exn(`Could not determine string representation for the value '${stringify(val)}' of '${paramName}' parameter.`)
  }

let valToCode = (~paramName="unknown-parameter", possibleValues:array<('a,string,string)>, val:'a): string  => 
  switch possibleValues->Array.getBy(((v,_,_)) => v==val)->Option.map(((_,_,c)) => c) {
    | Some(code) => code
    | None => exn(`Could not determine code representation for the value '${stringify(val)}' of '${paramName}' parameter.`)
  }

let rndEnumParam = (~paramName:string, ~defaultValue:'a, ~value:option<'a>, ~setValue:option<'a>=>unit, ~possibleValues:array<('a,string,string)>) => {
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
            | None => valToStr(possibleValues, defaultValue, ~paramName)
            | Some(val) => valToStr(possibleValues, val, ~paramName)
          }}
          onChange=evt2str(str => setValue(Some(strToVal(possibleValues, str, ~paramName))) )
        >
        {React.array(
          possibleValues->Js_array2.map(((_,s,_)) =>
            <MenuItem key=s value=s>{React.string(s)}</MenuItem>
          )
        )}
        </Select>
      </FormControl>
    }
  )
}

let strOrNull = Option.getWithDefault(_, "")