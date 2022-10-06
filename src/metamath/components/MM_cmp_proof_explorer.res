open Expln_React_Mui
open Expln_utils_common
open MM_parser

@react.component
let make = (~mmFile:stmt) => {
    Js.Console.log2("mmFile", mmFile)
    <Col>
        {React.string(`MM file was parsed successfully.`)}
    </Col>
}