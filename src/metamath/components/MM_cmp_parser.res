open Expln_React_common
open Expln_React_Mui
open MM_parser

@react.component
let make = () => {
    let (mmFileContent, setMmFileContent) = useState(None)
    let (mmFileParsed, setMmFileParsed) = useState(None)
    let (parseError, setParseError) = useState(None)

    let loadMmFileContent = text => {
        setMmFileContent(Some(text))
        switch parseMmFile(text) {
            | Ok(stmt) => setMmFileParsed(Some(stmt))
            | Error(msg) => setParseError(Some(msg))
        }
    }
    let rndMmFile = (~ast) => {
        <MM_cmp_proof_explorer mmFileContent={mmFileContent->Belt.Option.getExn} ast />
    }

    let rndMmFileContentOrError = () => {
        switch parseError {
            | Some(msg) =>
                <pre style=ReactDOM.Style.make(~color="red", ())>
                    {React.string("Parse error: " ++ msg)}
                </pre>
            | None => switch mmFileParsed {
                | Some(ast) => rndMmFile(~ast)
                | _ => React.null
            }
        }
    }

    <Col>
        <TextFileReader onChange=loadMmFileContent />
        {rndMmFileContentOrError()}
    </Col>
}