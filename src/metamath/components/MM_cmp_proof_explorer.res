open Expln_React_Mui
open Expln_utils_common
open MM_parser
open MM_proof_verifier

@react.component
let make = (~mmFileContent:string, ~ast:mmAstNode) => {
    React.useEffect0(() => {
        switch createContext(ast) {
            | Error((msg,pos)) => Js.log("Error processing mm file: " ++ msg ++ " at:" ++ textAt(mmFileContent, pos))
            | Ok(ctx) => Js.Console.log2("ctx", ctx)
        }
        Some(() => ())
    })
    <Col>
        {React.string(`MM file was parsed successfully.`)}
    </Col>
}