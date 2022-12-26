open MM_context
open Expln_React_common
open Expln_React_Mui
open Modal
open MM_wrk_editor
open MM_wrk_settings

type userStmtLocStor = {
    id: string,

    label: string,
    typ: string,
    cont: string,
    
    jstfText: string,
}   

type editorStateLocStor = {
    constsText: string,
    varsText: string,
    disjText: string,

    nextStmtId: int,
    stmts: array<userStmtLocStor>,
}

let userStmtLocStorToUserStmt = (userStmtLocStor:userStmtLocStor):userStmt => {
    {
        id: userStmtLocStor.id,

        label: userStmtLocStor.label,
        labelEditMode: false,
        typ: userStmtTypeFromStr(userStmtLocStor.typ),
        typEditMode: false,
        cont: strToCont(userStmtLocStor.cont),
        contEditMode: false,

        jstfText: userStmtLocStor.jstfText,
        jstfEditMode: false,

        stmtErr: None,

        expr: None,
        jstf: None,
        proof: None,
    }
}

let createInitialEditorState = (settingsV, settings, preCtxV, preCtx, stateLocStor:option<editorStateLocStor>) => {
    {
        settingsV,
        settings,

        preCtxV,
        preCtx,

        constsText: stateLocStor->Belt.Option.map(obj => obj.constsText)->Belt.Option.getWithDefault(""),
        constsEditMode: false,
        constsErr: None,

        varsText: stateLocStor->Belt.Option.map(obj => obj.varsText)->Belt.Option.getWithDefault(""),
        varsEditMode: false,
        varsErr: None,

        disjText: stateLocStor->Belt.Option.map(obj => obj.disjText)->Belt.Option.getWithDefault(""),
        disjEditMode: false,
        disjErr: None,
        disj: Belt_MapInt.fromArray([]),

        wrkCtx: None,

        nextStmtId: stateLocStor->Belt.Option.map(obj => obj.nextStmtId)->Belt.Option.getWithDefault(0),
        stmts: 
            stateLocStor
                ->Belt.Option.map(obj => obj.stmts->Js_array2.map(userStmtLocStorToUserStmt))
                ->Belt.Option.getWithDefault([]),
        checkedStmtIds: [],
    }
}

let editorStateToEditorStateLocStor = (state:editorState):editorStateLocStor => {
    {
        constsText: state.constsText,
        varsText: state.varsText,
        disjText: state.disjText,
        nextStmtId: state.nextStmtId,
        stmts: state.stmts->Js_array2.map(stmt => {
            {
                id: stmt.id,
                label: stmt.label,
                typ: (stmt.typ :> string),
                cont: contToStr(stmt.cont),
                jstfText: stmt.jstfText,
            }
        }),
    }
}

let editorSaveStateToLocStor = (state:editorState, key:string):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, Expln_utils_common.stringify(state->editorStateToEditorStateLocStor))
}

let readEditorStateFromLocStor = (key:string):option<editorStateLocStor> => {
    switch Dom_storage2.localStorage->Dom_storage2.getItem(key) {
        | None => None
        | Some(stateLocStorStr) => {
            open Expln_utils_jsonParse
            let parseResult = parseObj(stateLocStorStr, d=>{
                {
                    constsText: d->strOpt("constsText")->Belt_Option.getWithDefault(""),
                    varsText: d->strOpt("varsText")->Belt_Option.getWithDefault(""),
                    disjText: d->strOpt("disjText")->Belt_Option.getWithDefault(""),
                    nextStmtId: d->int("nextStmtId"),
                    stmts: d->arr("stmts", d=>{
                        {
                            id: d->str("id"),
                            label: d->str("label"),
                            typ: d->str("typ"),
                            cont: d->str("cont"),
                            jstfText: d->str("jstfText")
                        }
                    })
                }
            })
            switch parseResult {
                | Error(_) => None
                | Ok(res) => Some(res)
            }
        }
    }
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool) => {
    <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
}

let stateLocStorKey = "editor-state"

@react.component
let make = (~modalRef:modalRef, ~settingsV:int, ~settings:settings, ~preCtxV:int, ~preCtx:mmContext, ~top:int) => {
    let (state, setStatePriv) = React.useState(_ => createInitialEditorState(
        settingsV, settings, preCtxV, preCtx, readEditorStateFromLocStor(stateLocStorKey)
    ))

    let setState = (update:editorState=>editorState) => {
        setStatePriv(prev => {
            let newSt = update(prev)
            let newSt = validateSyntax(newSt)
            editorSaveStateToLocStor(newSt, stateLocStorKey)
            newSt
        })
    }

    let actSettingsUpdated = (settingsV, settings) => {
        setState(setSettings(_, settingsV, settings))
    }

    React.useEffect1(() => {
        actSettingsUpdated(settingsV, settings)
        None
    }, [settingsV])

    let actPreCtxUpdated = (preCtxV, preCtx) => {
        setState(setPreCtx(_, preCtxV, preCtx))
    }

    React.useEffect1(() => {
        actPreCtxUpdated(preCtxV, preCtx)
        None
    }, [preCtxV])

    let thereAreSyntaxErrors = editorStateHasErrors(state)
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
        state.constsEditMode || state.varsEditMode ||
        state.stmts->Js.Array2.some(stmt => stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode )

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
    let actBeginEdit0 = (setter:editorState=>editorState) => {
        if (!editIsActive) {
            setState(setter)
        }
    }
    let actBeginEdit = (setter:(editorState,string)=>editorState, stmtId:string) => {
        if (!editIsActive) {
            setState(setter(_,stmtId))
        }
    }
    let actCompleteEdit = (setter:editorState=>editorState) => {
        setState(setter)
    }

    let actUnify = () => {
        setState(validateSyntax)
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
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
                {
                    rndIconButton(~icon=<Icons2.Hub/>, ~onClick=actUnify, 
                        ~active= !editIsActive && !(mainCheckboxState->Belt_Option.getWithDefault(true)) && !thereAreSyntaxErrors )
                }
            </Row>
        </Paper>
    }

    let rndStmt = (stmt:userStmt) => {
        <Row alignItems=#"flex-start" key=stmt.id>
            <Checkbox
                disabled=editIsActive
                checked={state->isStmtChecked(stmt.id)}
                onChange={_ => actToggleStmtChecked(stmt.id)}
            />
            <Col>
                <MM_cmp_user_stmt
                    stmt

                    onLabelEditRequested={() => actBeginEdit(setLabelEditMode,stmt.id)}
                    onLabelEditDone={newLabel => actCompleteEdit(completeLabelEditMode(_,stmt.id,newLabel))}

                    onTypEditRequested={() => actBeginEdit(setTypEditMode,stmt.id)}
                    onTypEditDone={newTyp => actCompleteEdit(completeTypEditMode(_,stmt.id,newTyp))}

                    onContEditRequested={() => actBeginEdit(setContEditMode,stmt.id)}
                    onContEditDone={newCont => actCompleteEdit(completeContEditMode(_,stmt.id,newCont))}
                    
                    onJstfEditRequested={() => actBeginEdit(setJstfEditMode,stmt.id)}
                    onJstfEditDone={newJstf => actCompleteEdit(completeJstfEditMode(_,stmt.id,newJstf))}
                />
                {rndError(stmt.stmtErr)}
            </Col>
        </Row>
    }

    let rndConsts = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Constants")}
            <Col>
                <MM_cmp_multiline_text
                    text=state.constsText
                    editMode=state.constsEditMode
                    onEditRequested={() => actBeginEdit0(setConstsEditMode)}
                    onEditDone={newText => actCompleteEdit(completeConstsEditMode(_,newText))}
                />
                {rndError(state.constsErr)}
            </Col>
        </Row>
    }

    let rndVars = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Variables")}
            <Col>
                <MM_cmp_multiline_text
                    text=state.varsText
                    editMode=state.varsEditMode
                    onEditRequested={() => actBeginEdit0(setVarsEditMode)}
                    onEditDone={newText => actCompleteEdit(completeVarsEditMode(_,newText))}
                />
                {rndError(state.varsErr)}
            </Col>
        </Row>
    }

    let rndDisj = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Disjoints")}
            <Col>
                <MM_cmp_multiline_text
                    text=state.disjText
                    editMode=state.disjEditMode
                    onEditRequested={() => actBeginEdit0(setDisjEditMode)}
                    onEditDone={newText => actCompleteEdit(completeDisjEditMode(_,newText))}
                />
                {rndError(state.disjErr)}
            </Col>
        </Row>
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
                {rndVars()}
                {rndDisj()}
                {rndStmts()}
            </Col>
        }}
    />
}