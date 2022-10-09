open Expln_React_Mui
open Expln_utils_common
open MM_parser
open MM_context

@react.component
let make = (~mmFileContent:string, ~ast:mmAstNode) => {
    React.useEffect0(() => {
        try {
            let _ = createContext(ast)
            Js.log(`MM context was created successfully.`)
        } catch {
            | MmException(ex) =>
                Js.log(
                    "Error processing mm file: "
                        ++ ex.msg ++ " at:"
                        ++ ex.begin->Belt.Option.map(textAt(mmFileContent, _))->Belt_Option.getWithDefault("No location info is available.")
                )
        }
        Some(() => ())
    })
    <Col>
        {React.string(`MM file was parsed successfully.`)}
    </Col>
}