open Expln_React_Mui
open Expln_utils_common
open MM_parser
open MM_proof_verifier

@react.component
let make = (~mmFileContent:string, ~ast:mmAstNode) => {
    React.useEffect0(() => {
        try {
            let _ = createContext(ast)
            Js.log(`MM context was created successfully.`)
        } catch {
            | MmException(ex) => Js.log("Error processing mm file: " ++ ex.msg ++ " at:" ++ textAt(mmFileContent, ex.begin->Belt_Option.getWithDefault(-1)))
        }
        Some(() => ())
    })
    <Col>
        {React.string(`MM file was parsed successfully.`)}
    </Col>
}