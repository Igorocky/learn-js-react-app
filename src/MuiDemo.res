open Expln_React_common
open Expln_React_Mui
open Belt

@react.component
let make = () => {
  let (value, setValue) = useState(None)


  

  <Col justifyContent=#"flex-start" alignItems=#center spacing=2. style=ReactDOM.Style.make(~padding="10px", ())>
    <Row>
      <Paper style=ReactDOM.Style.make(~padding="50px", ()) >
        <TextField value={value->Option.getWithDefault("")} />
      </Paper>
    </Row>
    <Row alignItems=#center spacing=2.>
      <Checkbox checked={Option.isSome(value)} onChange=evt2Bool(checked => setValue(if checked {Some("Value")} else {None})) />
      {React.string("value")}
      <TextField disabled={value->Option.isNone} value={value->Option.getWithDefault("")} onChange=evt2Str(setValue) />
    </Row>
  </Col>
}
