open Expln_React_common
open Expln_React_Mui
open MM_parser

@react.component
let make = () => {
    let (mmFileText, setMmFileText) = useState(None)
    let (mmAst, setMmAst) = useState(None)
    let (mmCtx, setMmCtx) = useState(None)
    let (errorMsg, setErrorMsg) = useState(None)

    let loadMmFileText = text => {
        setMmFileText(Some(text))
        try {
            let ast = parseMmFile(text)
            setMmAst(Some(ast))
            setErrorMsg(None)
        } catch {
            | MmException({msg}) => setErrorMsg(Some(msg))
        }
    }

    let rndErrorIfAny = () => {
        switch errorMsg {
            | None => React.null
            | Some(msg) => {
                <pre style=ReactDOM.Style.make(~color="red", ())>
                    {React.string("Error: " ++ msg)}
                </pre>
            }
        }
    }

    let rndInitialFileSelector = () => {
        <Col justifyContent=#center alignItems=#center style=ReactDOM.Style.make(~marginTop="100px", ()) >
            {rndErrorIfAny()}
            <TextFileReader onChange=loadMmFileText />
        </Col>
    }

    let rndCtxSettings = () => {
        React.string("Ctx Settings")
    }

    let rndTabs = () => {
        React.string("Tabs")
    }

    switch mmAst {
        | None => rndInitialFileSelector()
        | Some(_) => {
            switch mmCtx {
                | None => rndCtxSettings()
                | Some(_) => rndTabs()
            }
        }
    }
}