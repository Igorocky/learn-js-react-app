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
let make = (~modalRef:modalRef, ~settingsV:int, ~settings:settings, ~ctxV:int, ~ctx:mmContext, ~top:int) => {
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

    let actAddNewStmt = () => setState(addNewStmt)
    let actDeleteCheckedStmts = () => setState(deleteCheckedStmts)
    let actToggleStmtChecked = id => setState(toggleStmtChecked(_,id))
    let actToggleMainCheckbox = () => {
        switch mainCheckboxState {
            | Some(true) | None => setState(uncheckAllStmts)
            | Some(false) => setState(checkAllStmts)
        }
    }
    let actMoveCheckedStmtsUp = () => setState(moveCheckedStmts(_, true))
    let actMoveCheckedStmtsDown = () => setState(moveCheckedStmts(_, false))
    let actDuplicateStmt = () => setState(duplicateCheckedStmt)

    let rndButtons = () => {
        <Row>
            <Checkbox
                indeterminate={mainCheckboxState->Belt_Option.isNone}
                checked={mainCheckboxState->Belt_Option.getWithDefault(false)}
                onChange={_ => actToggleMainCheckbox()}
            />
            <IconButton disabled={!canMoveCheckedStmts(state,false)} onClick={_ => actMoveCheckedStmtsDown()}> <Icons2.ArrowDownward/> </IconButton>
            <IconButton disabled={!canMoveCheckedStmts(state,true)} onClick={_ => actMoveCheckedStmtsUp()}> <Icons2.ArrowUpward/> </IconButton>
            <IconButton onClick={_ => actAddNewStmt()}> <Icons2.Add/> </IconButton>
            <IconButton 
                disabled={!(mainCheckboxState->Belt.Option.getWithDefault(true))} 
                onClick={_ => actDeleteCheckedStmts()}
            > 
                <Icons2.DeleteForever/> 
            </IconButton>
            <IconButton 
                disabled={!isSingleStmtChecked(state)} 
                onClick={_ => actDuplicateStmt()}
            > 
                <Icons2.FilterNone/> 
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

    <>
        <div style=ReactDOM.Style.make(~position="sticky", ~top=`${top->Belt_Int.toString}px`, ())>
            {rndButtons()}
        </div>
        {rndStmts()}
    </>
}