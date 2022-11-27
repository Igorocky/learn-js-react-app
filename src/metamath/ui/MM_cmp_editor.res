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

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool) => {
    <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
}

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

    let editIsActive = 
        state.constsEditMode ||
        state.stmts->Js.Array2.some(stmt => stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.proofEditMode )

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
    let actBeginEditConsts = setter => {
        if (!editIsActive) {
            setState(setter)
        }
    }
    let actBeginEdit = (setter, stmtId) => {
        if (!editIsActive) {
            setState(setter(_,stmtId))
        }
    }

    let rndButtons = () => {
        <Paper>
            <Row>
                <Checkbox
                    disabled=editIsActive
                    indeterminate={mainCheckboxState->Belt_Option.isNone}
                    checked={mainCheckboxState->Belt_Option.getWithDefault(false)}
                    onChange={_ => actToggleMainCheckbox()}
                />
                {rndIconButton(~icon=<Icons2.ArrowDownward/>, ~onClick=actMoveCheckedStmtsDown, ~active= !editIsActive && canMoveCheckedStmts(state,false))}
                {rndIconButton(~icon=<Icons2.ArrowUpward/>, ~onClick=actMoveCheckedStmtsUp, ~active= !editIsActive && canMoveCheckedStmts(state,true))}
                {rndIconButton(~icon=<Icons2.Add/>, ~onClick=actAddNewStmt, ~active= !editIsActive)}
                {rndIconButton(~icon=<Icons2.DeleteForever/>, ~onClick=actDeleteCheckedStmts, 
                    ~active= !editIsActive && mainCheckboxState->Belt.Option.getWithDefault(true)
                )}
                {rndIconButton(~icon=<Icons2.ControlPointDuplicate/>, ~onClick=actDuplicateStmt, ~active= !editIsActive && isSingleStmtChecked(state))}
            </Row>
        </Paper>
    }

    let rndStmt = stmt => {
        <Row alignItems=#"flex-start" key=stmt.id>
            <Checkbox
                disabled=editIsActive
                checked={state->isStmtChecked(stmt.id)}
                onChange={_ => actToggleStmtChecked(stmt.id)}
            />
            <MM_cmp_user_stmt
                stmt

                onLabelEditRequested={() => actBeginEdit(setLabelEditMode,stmt.id)}
                onLabelEditDone={newLabel => setState(completeLabelEditMode(_,stmt.id,newLabel))}

                onTypEditRequested={() => actBeginEdit(setTypEditMode,stmt.id)}
                onTypEditDone={newTyp => setState(completeTypEditMode(_,stmt.id,newTyp))}

                onContEditRequested={() => actBeginEdit(setContEditMode,stmt.id)}
                onContEditDone={newCont => setState(completeContEditMode(_,stmt.id,newCont))}
                
                onProofEditRequested={() => actBeginEdit(setProofEditMode,stmt.id)}
                onProofEditDone={newProof => setState(completeProofEditMode(_,stmt.id,newProof))}
            />
        </Row>
    }

    let rndConsts = () => {
        <MM_cmp_user_consts
            constsText=state.constsText
            constsEditMode=state.constsEditMode
            onConstsEditRequested={() => actBeginEditConsts(setConstsEditMode)}
            onConstsEditDone={newConstsText => setState(completeConstsEditMode(_,newConstsText))}
        />
    }

    let rndStmts = () => {
        <Col>
            { state.stmts->Js_array2.map(rndStmt)->React.array }
        </Col>
    }

    <ContentWithStickyHeader 
        top
        header={rndButtons()}
        content={_ => {
            <Col>
                {rndConsts()}
                {rndStmts()}
            </Col>
        }}
    />
}