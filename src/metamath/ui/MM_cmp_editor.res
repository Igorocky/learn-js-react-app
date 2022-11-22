open MM_context
open Expln_React_common
open Expln_React_Mui
open MM_wrk_FindParens
open Modal
open Expln_utils_promise
open MM_cmp_settings
open MM_id_generator
open MM_parser
open MM_syntax_tree
open MM_wrk_editor
open Expln_React_common
open Expln_React_Mui


@react.component
let make = (~modalRef:modalRef, ~settingsV:int, ~settings:settings, ~ctxV:int, ~ctx:mmContext) => {
    let (state, setState) = React.useState(_ => initialState)

    let mainCheckboxState = {
        let atLeastOneStmtIsChecked = state.checkedStmtIds->Js.Array2.length != 0
        let atLeastOneStmtIsNotChecked = state.stmts->Js.Array2.length != state.checkedStmtIds->Js.Array2.length
        if (atLeastOneStmtIsChecked && atLeastOneStmtIsNotChecked) {
            None
        } else if (atLeastOneStmtIsChecked && !atLeastOneStmtIsNotChecked) {
            Some(true)
        } else {
            Some(false)
        }
    }

    let actToggleStmtChecked = id => setState(toggleStmtChecked(_,id))

    let actToggleMainCheckbox = () => {
        switch mainCheckboxState {
            | Some(true) | None => setState(uncheckAllStmts)
            | Some(false) => setState(checkAllStmts)
        }
    }

    let rndButtons = () => {
        <Row>
            <Checkbox
                indeterminate={mainCheckboxState->Belt_Option.isNone}
                checked={mainCheckboxState->Belt_Option.getWithDefault(false)}
                onChange={_ => actToggleMainCheckbox()}
            />
            <IconButton key="add-button" onClick={_ => setState(addNewStmt)}>
                <Icons2.Add/>
            </IconButton>
        </Row>
    }

    let rndStmt = stmt => {
        <Paper key=stmt.id>
            <Row>
                <Checkbox
                    checked={state->isStmtChecked(stmt.id)}
                    onChange={_ => actToggleStmtChecked(stmt.id)}
                />
                {stmt.id->React.string}
            </Row>
        </Paper>
    }

    let rndStmts = () => {
        <Col>
            { state.stmts->Js_array2.map(rndStmt)->React.array }
        </Col>
    }

    <Col>
        {rndButtons()}
        {rndStmts()}
    </Col>
}