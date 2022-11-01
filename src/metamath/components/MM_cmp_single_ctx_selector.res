open Expln_React_common
open Expln_React_Mui
open MM_parser

type readInstr =
    | All
    | StopBefore
    | StopAfter

let riAll = "all"
let riStopBefore = "stopBefore"
let riStopAfter = "stopAfter"

let readInstrToStr = ri => {
    switch ri {
        | All => riAll
        | StopBefore => riStopBefore
        | StopAfter => riStopAfter
    }
}
let readInstrFromStr = ri => {
    if (ri == riAll) {
        All
    } else if (ri == riStopBefore) {
        StopBefore
    } else if (ri == riStopAfter) {
        StopAfter
    } else {
        raise(MmException({msg:`Unexpected read instruction: ${ri}`}))
    }
}

type mmSingleScope = {
    id:string,
    fileName: option<string>,
    fileText: option<string>,
    ast: option<result<mmAstNode,string>>,
    allLabels: array<string>,
    readInstr: readInstr,
    label: option<string>,
}

@react.component
let make = (~initialScope:mmSingleScope, ~onChange:mmSingleScope=>unit, ~onDelete:unit=>unit, ~renderDeleteButton:bool) => {
    let (fileName, setFileName) = useState(initialScope.fileName)
    let (fileText, setFileText) = useState(initialScope.fileText)
    let (ast, setAst) = useState(initialScope.ast)
    let (allLabels, setAllLabels) = useState(initialScope.allLabels)
    let (readInstr, setReadInstr) = useState(initialScope.readInstr)
    let (label, setLabel) = useState(initialScope.label)

    let setScope = (~fileName, ~fileText, ~ast, ~allLabels, ~readInstr, ~label) => {
        setFileName(fileName)
        setFileText(fileText)
        setAst(ast)
        setAllLabels(allLabels)
        setReadInstr(readInstr)
        setLabel(label)
        onChange({
            id:initialScope.id,
            fileName,
            fileText,
            ast,
            allLabels,
            readInstr,
            label,
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

    let loadMmFileText = (nameAndTextOpt) => {
        switch nameAndTextOpt {
            | None => {
                setScope(~fileName=None, ~fileText=None, ~ast=None, ~allLabels=[], ~readInstr=All, ~label=None)
            }
            | Some((name,text)) => {
                let fileName = Some(name)
                let fileText = Some(text)
                try {
                    let astRootNode = parseMmFile(text)
                    let ast = Some(Ok(astRootNode))
                    let allLabels:array<string> = extractAllLabels(astRootNode)->Js_array2.sortInPlace
                    let readInstr = All
                    let label = None
                    setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr, ~label)
                } catch {
                    | MmException({msg}) => {
                        let ast = Some(Error(msg))
                        let allLabels = []
                        let readInstr = All
                        let label = None
                        setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr, ~label)
                    }
                }
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
                    setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr=readInstrFromStr(newReadInstrType), ~label)
                })
            >
                <MenuItem value=riAll>{React.string("Read all")}</MenuItem>
                <MenuItem value=riStopBefore>{React.string("Stop before")}</MenuItem>
                <MenuItem value=riStopAfter>{React.string("Stop after")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndLabelSelector = () => {
        <AutocompleteVirtualized value=label options=allLabels size=#small
            onChange={newLabel => setScope(~fileName, ~fileText, ~ast, ~allLabels, ~readInstr, ~label=newLabel)}
        />
    }

    <Row alignItems=#center spacing=1. >
        {
            if (renderDeleteButton) {
                <IconButton onClick={_ => onDelete()} >
                    <Icons.Delete/>
                </IconButton>
            } else {
                React.null
            }
        }
        <TextFileReader2 onChange=loadMmFileText />
        {
            switch ast {
                | None => React.null
                | Some(Error(msg)) => {
                    <pre style=ReactDOM.Style.make(~color="red", ())>
                        {React.string("Error: " ++ msg)}
                    </pre>
                }
                | Some(Ok(_)) => {
                    <Row>
                        {rndReadInstrTypeSelector()}
                        {
                            switch readInstr {
                                | StopBefore | StopAfter => {
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