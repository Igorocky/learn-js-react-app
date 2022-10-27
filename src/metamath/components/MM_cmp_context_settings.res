open Expln_React_common
open Expln_React_Mui
open MM_parser

type scope =
    | All
    | StopBefore({label:option<string>})
    | StopAfter({label:option<string>})

let stAll = "stAll"
let stStopBefore = "stStopBefore"
let stStopAfter = "stStopAfter"

let scopeTypeToStr = scope => {
    switch scope {
        | All => stAll
        | StopBefore(_) => stStopBefore
        | StopAfter(_) => stStopAfter
    }
}
let scopeTypeFromStr = str => {
    if (str == stAll) {
        All
    } else if (str == stStopBefore) {
        StopBefore({label:None})
    } else if (str == stStopAfter) {
        StopAfter({label:None})
    } else {
        raise(MmException({msg:`Unexpected scope types string: ${str}`}))
    }
}

@react.component
let make = () => {
    let (scope, setScope) = useState(All)

    <FormControl>
        <InputLabel id="scope-type-select-label"> "Scope" </InputLabel>
        <Select 
            labelId="scope-type-select-label"
            value=scopeTypeToStr(scope)
            label="Scope"
            onChange=evt2Str(str => setScope(scopeTypeFromStr(str)))
        >
            <MenuItem value=stAll> {React.string("Read all file")} </MenuItem>
            <MenuItem value=stStopBefore> {React.string("Stop before")} </MenuItem>
            <MenuItem value=stStopAfter> {React.string("Stop after")} </MenuItem>
        </Select>
    </FormControl>
}