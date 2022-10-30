open Expln_React_common
open Expln_React_Mui
open MM_parser

type readInstr =
    | All
    | StopBefore(string)
    | StopAfter(string)

let riAll = "all"
let riStopBefore = "stopBefore"
let riStopAfter = "stopAfter"

let readInstrToStr = ri => {
    switch ri {
        | All => riAll
        | StopBefore(_) => riStopBefore
        | StopAfter(_) => riStopAfter
    }
}
let readInstrFromStr = (ri,label) => {
    if (ri == riAll) {
        All
    } else if (ri == riStopBefore) {
        StopBefore(label)
    } else if (ri == riStopAfter) {
        StopAfter(label)
    } else {
        raise(MmException({msg:`Unexpected read instruction: ${ri}`}))
    }
}

type mmSingleScope = {
    fileName: option<string>,
    fileText: option<string>,
    ast: option<result<mmAstNode,string>>,
    allLabels: array<string>,
    readInstr: readInstr,
}

@react.component
let make = (~initialScope:mmSingleScope, ~onChange:mmSingleScope=>unit) => {
    let (fileName, setFileName) = useState(initialScope.fileName)
    let (fileText, setFileText) = useState(initialScope.fileText)
    let (ast, setAst) = useState(initialScope.ast)
    let (allLabels, setAllLabels) = useState(initialScope.allLabels)
    let (readInstr, setReadInstr) = useState(initialScope.readInstr)

    let setScope = (~fileName, ~fileText, ~ast, ~allLabels, ~readInstr) => {
        setFileName(fileName)
        setFileText(fileText)
        setAst(ast)
        setAllLabels(allLabels)
        setReadInstr(readInstr)
        onChange({
            fileName,
            fileText,
            ast,
            allLabels,
            readInstr,
        })
    }

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

    let loadMmFileText = (name, text) => {
        let fileName = Some(name)
        let fileText = Some(text)
        try {
            let astRootNode = parseMmFile(text)
            let ast = Some(Ok(astRootNode))
            let allLabels:array<string> = extractAllLabels(astRootNode)->Js_array2.sortInPlace
            let readInstr = All
            setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr)
        } catch {
            | MmException({msg}) => {
                let ast = Some(Error(msg))
                let allLabels = []
                let readInstr = All
                setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr)
            }
        }
    }

    let rndReadInstrTypeSelector = () => {
        <FormControl size=#small>
            <InputLabel id="scope-type-select-label">"Scope"</InputLabel>
            <Select 
                labelId="scope-type-select-label"
                value=readInstrToStr(readInstr)
                label="Scope"
                onChange=evt2Str(newReadInstrType => {
                    setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr=readInstrFromStr(newReadInstrType, ""))
                })
            >
                <MenuItem value=riAll>{React.string("Read all")}</MenuItem>
                <MenuItem value=riStopBefore>{React.string("Stop before")}</MenuItem>
                <MenuItem value=riStopAfter>{React.string("Stop after")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndLabelSelector = () => {
        <AutocompleteVirtualized options=allLabels size=#small
            onChange={newLabelOpt => {
                let newLabelStr = switch newLabelOpt {
                    | Some(newLabel) => newLabel
                    | None => ""
                }
                setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr=readInstrFromStr(readInstrToStr(readInstr), newLabelStr))
            }}
        />
    }

    let rndFileSelector = () => {
        <Row alignItems=#center >
            <TextFileReader2 onChange=loadMmFileText />
            {
                switch ast {
                    | None => React.null
                    | Some(Error(msg)) => {
                        <pre style=ReactDOM.Style.make(~color="red", ())>
                            {React.string("Error: " ++ msg)}
                        </pre>
                    }
                    | Some(Ok(ast)) => {
                        <Row>
                            {rndReadInstrTypeSelector()}
                            {
                                switch readInstr {
                                    | StopBefore(_) | StopAfter(_) => {
                                        rndLabelSelector()
                                    }
                                    | _ => React.null
                                }
                            }
                        </Row>
                    }
                }
            }
        </Row>
    }

    <Col>
        {rndFileSelector()}
    </Col>
}