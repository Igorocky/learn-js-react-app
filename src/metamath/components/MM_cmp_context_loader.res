open Expln_React_common
open Expln_React_Mui
open MM_parser

let extractAllLabels = ast => {
    let result = []
    traverseAst((), ast, ~process = (_,n) => {
        switch n {
            | {stmt:Axiom({label})} | {stmt:Provable({label})} => result->Js_array2.push(label)->ignore
            | _ => ()
        }
        None
    }, ())->ignore
    result
}

@react.component
let make = () => {
    let (mmFileText, setMmFileText) = useState(None)
    let (mmAst, setMmAst) = useState(None)
    let (mmCtx, setMmCtx) = useState(None)
    let (errorMsg, setErrorMsg) = useState(None)

    let (allLabels, setAllLabels) = useState([])

    let loadMmFileText = text => {
        setMmFileText(Some(text))
        try {
            let ast = parseMmFile(text)
            setMmAst(Some(ast))
            setErrorMsg(None)
            setAllLabels(extractAllLabels(ast)->Js_array2.sortInPlace)
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
        <Col justifyContent=#center alignItems=#center style=ReactDOM.Style.make(~marginTop="100px", ()) >
            <MM_cmp_context_settings allLabels />
        </Col>
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