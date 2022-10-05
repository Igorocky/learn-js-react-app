open Expln_React_common
open Expln_React_Mui
open MM_parsers
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
    let rndMmFile = (~mmFile) => {
        <MM_cmp_proof_explorer mmFile />
    }

    let rndMmFileContentOrError = () => {
        switch parseError {
            | Some(msg) =>
                <pre style=ReactDOM.Style.make(~color="red", ())>
                    {React.string("Parse error: " ++ msg)}
                </pre>
            | None => switch mmFileParsed {
                | Some(mmFile) => rndMmFile(~mmFile)
                | _ => React.null
            }
        }
    }

    <Col>
        <TextFileReader onChange=loadMmFileContent />
        {rndMmFileContentOrError()}
    </Col>
}