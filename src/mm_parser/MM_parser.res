open Expln_React_common
open Expln_React_Mui
open MM_parsers

@react.component
let make = () => {
    let (mmFileContent, setMmFileContent) = useState(None)
    let (comments, setComments) = useState(None)
    let (nonComments, setNonComments) = useState(None)
    let (parseError, setParseError) = useState(None)

    let loadMmFileContent = text => {
        setMmFileContent(Some(text))
        switch extractComments(text) {
            | Ok({result:(comments, nonComments)}) => {
                setComments(Some(comments))
                setNonComments(Some(nonComments))
            }
            | Error(msg) => setParseError(Some(msg))
        }
    }
    let rndMmFile = (~comments, ~nonComments) => {
        <MM_proof_explorer comments nonComments />
    }

    let rndMmFileContentOrError = () => {
        switch parseError {
            | Some(msg) =>
                <div style=ReactDOM.Style.make(~color="red", ())>
                    {React.string("Parse error: " ++ msg)}
                </div>
            | None => switch (comments, nonComments) {
                | (Some(comments), Some(nonComments)) => rndMmFile(~comments, ~nonComments)
                | _ => React.null
            }
        }
    }

    <Col>
        <TextFileReader onChange=loadMmFileContent />
        {rndMmFileContentOrError()}
    </Col>
}