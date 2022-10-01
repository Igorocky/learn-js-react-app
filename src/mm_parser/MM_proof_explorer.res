open MM_types
open Expln_React_Mui
open Expln_utils_common

@react.component
let make = (~comments:array<comment>, ~nonComments:array<nonComment>) => {
    <Col>
        {React.string(`Comments: ${comments->Js_array2.length->i2s}`)}
        {React.string(`Non-comments: ${nonComments->Js_array2.length->i2s}`)}
    </Col>
}