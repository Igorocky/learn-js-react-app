open MM_context
open Expln_React_common
open Expln_React_Mui
open Modal
open MM_cmp_settings
open MM_parser
open MM_cmp_user_stmt

type userStmtLocStor = {
    id: string,

    label: string,
    typ: string,
    cont: string,
    
    proof: string,
}   

let createEmptyUserStmt = (id, typ):userStmt => {
    { 
        id, 
        label:"label", labelEditMode:false, 
        typ, typEditMode:false, 
        cont:Text({text:[], syntaxError:None}), contEditMode:true, 
        proof:"", proofEditMode:false,  proofError:None 
    }
}

type state = {
    settingsV:int,
    settings:settings,

    ctxV: int,
    ctx: mmContext,

    constsText: string,
    consts: array<string>,
    constsErr: option<string>,
    constsEditMode: bool,

    varsText: string,
    vars: array<stmt>,
    varsErr: option<string>,
    varsEditMode: bool,

    disjText: string,
    disj: Belt_MapInt.t<Belt_SetInt.t>,
    disjErr: option<string>,
    disjEditMode: bool,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<string>,
}

type stateLocStor = {
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
        
        proof: userStmtLocStor.proof,
        proofEditMode: false,
        proofError: None,
    }
}

let createInitialState = (settingsV, settings, ctxV, ctx, stateLocStor:option<stateLocStor>) => {
    {
        settingsV,
        settings,

        ctxV,
        ctx,

        constsText: stateLocStor->Belt.Option.map(obj => obj.constsText)->Belt.Option.getWithDefault(""),
        consts: [],
        constsErr: None,
        constsEditMode: false,

        varsText: stateLocStor->Belt.Option.map(obj => obj.varsText)->Belt.Option.getWithDefault(""),
        vars: [],
        varsErr: None,
        varsEditMode: false,

        disjText: stateLocStor->Belt.Option.map(obj => obj.disjText)->Belt.Option.getWithDefault(""),
        disj: Belt_MapInt.fromArray([]),
        disjErr: None,
        disjEditMode: false,

        nextStmtId: stateLocStor->Belt.Option.map(obj => obj.nextStmtId)->Belt.Option.getWithDefault(0),
        stmts: 
            stateLocStor
                ->Belt.Option.map(obj => obj.stmts->Js_array2.map(userStmtLocStorToUserStmt))
                ->Belt.Option.getWithDefault([]),
        checkedStmtIds: [],
    }
}

let stateToStateLocStor = (state:state):stateLocStor => {
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
                proof: stmt.proof,
            }
        }),
    }
}

let editorSaveStateToLocStor = (state:state, key:string) => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, Expln_utils_common.stringify(state->stateToStateLocStor))
}

let editorReadStateFromLocStor = (key:string):option<stateLocStor> => {
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
                            proof: d->str("proof")
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

let updateStmt = (st:state,id,update):state => {
    {
        ...st,
        stmts: st.stmts->Js_array2.map(stmt => if stmt.id == id {update(stmt)} else {stmt})
    }
}

let isStmtChecked = (st,id) => {
    st.checkedStmtIds->Js.Array2.includes(id)
}

let toggleStmtChecked = (st,id) => {
    if (isStmtChecked(st,id)) {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Js_array2.filter(checkedId => checkedId != id)
        }
    } else {
        {
            ...st,
            checkedStmtIds: st.checkedStmtIds->Js_array2.concat([id])
        }
    }
}

let checkAllStmts = (st:state):state => {
    {
        ...st,
        checkedStmtIds: st.stmts->Js.Array2.map(stmt => stmt.id)
    }
}

let uncheckAllStmts = (st:state):state => {
    {
        ...st,
        checkedStmtIds: []
    }
}

let deleteCheckedStmts = (st:state):state => {
    let newStmts = st.stmts->Js_array2.filter(stmt => !isStmtChecked(st,stmt.id))
    let newNextStmtId = if (newStmts->Js_array2.length == 0) { 0 } else { st.nextStmtId }
    {
        ...st,
        stmts: newStmts,
        checkedStmtIds: [],
        nextStmtId: newNextStmtId,
    }
}

let canMoveCheckedStmts = (st:state, up):bool => {
    let len = st.stmts->Js_array2.length
    len != 0 && st.checkedStmtIds->Js_array2.length != 0 && (
        (up && !isStmtChecked(st,st.stmts[0].id)) || (!up && !isStmtChecked(st,st.stmts[len-1].id))
    )
}

let moveCheckedStmts = (st:state,up):state => {
    if (!canMoveCheckedStmts(st,up)) {
        st
    } else {
        let len = st.stmts->Js_array2.length
        let res = st.stmts->Js.Array2.copy
        if up {
            let maxI = len-2
            for i in 0 to maxI {
                if (!isStmtChecked(st,res[i].id) && isStmtChecked(st,res[i+1].id)) {
                    let tmp = res[i]
                    res[i] = res[i+1]
                    res[i+1] = tmp
                }
            }
        } else {
            for i in len-1 downto 1 {
                if (isStmtChecked(st,res[i-1].id) && !isStmtChecked(st,res[i].id)) {
                    let tmp = res[i]
                    res[i] = res[i-1]
                    res[i-1] = tmp
                }
            }
        }
        {
            ...st,
            stmts: res,
        }
    }
}

let addNewStmt = (st:state):state => {
    let newId = st.nextStmtId->Belt_Int.toString
    let idToAddBefore = st.stmts->Js_array2.find(stmt => st.checkedStmtIds->Js_array2.includes(stmt.id))->Belt_Option.map(stmt => stmt.id)
    {
        ...st,
        nextStmtId: st.nextStmtId+1,
        stmts: 
            switch idToAddBefore {
                | Some(idToAddBefore) => {
                    st.stmts->Js_array2.map(stmt => {
                        if (stmt.id == idToAddBefore) {
                            [createEmptyUserStmt(newId,#p), stmt]
                        } else {
                            [stmt]
                        }
                    })->Belt_Array.concatMany
                }
                | None => st.stmts->Js_array2.concat([createEmptyUserStmt(newId, #p)])
            }
    }
}

let isSingleStmtChecked = st => st.checkedStmtIds->Js_array2.length == 1

let duplicateCheckedStmt = st => {
    if (!isSingleStmtChecked(st)) {
        st
    } else {
        let newId = st.nextStmtId->Belt_Int.toString
        let idToAddAfter = st.checkedStmtIds[0]
        {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                st.stmts->Js_array2.map(stmt => {
                    if (stmt.id == idToAddAfter) {
                        [stmt, {...stmt, id:newId}]
                    } else {
                        [stmt]
                    }
                })->Belt_Array.concatMany,
            checkedStmtIds: [newId],
        }
    }
}

let canGoEditModeForStmt = (st:state,stmtId) => {
    !(st.stmts->Js_array2.some(stmt => stmt.id == stmtId && (stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.proofEditMode)))
}

let setConstsEditMode = st => {
    {
        ...st,
        constsEditMode: true
    }
}

let completeConstsEditMode = (st, newConstsText) => {
    {
        ...st,
        constsText:newConstsText,
        constsEditMode: false
    }
}

let setVarsEditMode = st => {
    {
        ...st,
        varsEditMode: true
    }
}

let completeVarsEditMode = (st, newVarsText) => {
    {
        ...st,
        varsText:newVarsText,
        varsEditMode: false
    }
}

let setDisjEditMode = st => {
    {
        ...st,
        disjEditMode: true
    }
}

let completeDisjEditMode = (st, newDisjText) => {
    {
        ...st,
        disjText:newDisjText,
        disjEditMode: false
    }
}

let setLabelEditMode = (st:state, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, labelEditMode:true})
    } else {
        st
    }
}

let completeLabelEditMode = (st, stmtId, newLabel) => {
    updateStmt(st, stmtId, stmt => {
        if (newLabel->Js_string2.trim != "") {
            {
                ...stmt,
                label:newLabel,
                labelEditMode: false
            }
        } else {
            stmt
        }
    })
}

let setContEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, contEditMode:true})
    } else {
        st
    }
}

let completeContEditMode = (st, stmtId, newCont) => {
    updateStmt(st, stmtId, stmt => {
        if (contIsEmpty(newCont)) {
            stmt
        } else {
            {
                ...stmt,
                cont:newCont,
                contEditMode: false
            }
        }
    })
}

let setTypEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, typEditMode:true})
    } else {
        st
    }
}

let completeTypEditMode = (st, stmtId, newTyp) => {
    updateStmt(st, stmtId, stmt => {
        {
            ...stmt,
            typ:newTyp,
            typEditMode: false
        }
    })
}

let setProofEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, proofEditMode:true})
    } else {
        st
    }
}

let completeProofEditMode = (st, stmtId, newProof) => {
    updateStmt(st, stmtId, stmt => {
        {
            ...stmt,
            proof:newProof,
            proofEditMode: false
        }
    })
}

let rndIconButton = (~icon:reElem, ~onClick:unit=>unit, ~active:bool) => {
    <IconButton disabled={!active} onClick={_ => onClick()} color="primary"> icon </IconButton>
}

let setSettings = (st, settingsV, settings) => {
    { ...st, settingsV, settings }
}

let setCtx = (st, ctxV, ctx) => {
    { ...st, ctxV, ctx }
}

let stateLocStorKey = "editor-state"

@react.component
let make = (~modalRef:modalRef, ~settingsV:int, ~settings:settings, ~ctxV:int, ~ctx:mmContext, ~top:int) => {
    let (state, setStatePriv) = React.useState(_ => createInitialState(settingsV, settings, ctxV, ctx, editorReadStateFromLocStor(stateLocStorKey)))

    let setState = update => {
        setStatePriv(prev => {
            let new = update(prev)
            new->editorSaveStateToLocStor(stateLocStorKey)
            new
        })
    }

    let actSettingsUpdated = (settingsV, settings) => {
        setState(setSettings(_, settingsV, settings))
    }

    React.useEffect1(() => {
        actSettingsUpdated(settingsV, settings)
        None
    }, [settingsV])

    let actCtxUpdated = (ctxV, ctx) => {
        setState(setCtx(_, ctxV, ctx))
    }

    React.useEffect1(() => {
        actCtxUpdated(ctxV, ctx)
        None
    }, [ctxV])

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
    let actBeginEdit0 = setter => {
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

    let rndStmt = (stmt:userStmt) => {
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
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Constants")}
            <MM_cmp_multiline_text
                text=state.constsText
                editMode=state.constsEditMode
                onEditRequested={() => actBeginEdit0(setConstsEditMode)}
                onEditDone={newText => setState(completeConstsEditMode(_,newText))}
            />
        </Row>
    }

    let rndVars = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Variables")}
            <MM_cmp_multiline_text
                text=state.varsText
                editMode=state.varsEditMode
                onEditRequested={() => actBeginEdit0(setVarsEditMode)}
                onEditDone={newText => setState(completeVarsEditMode(_,newText))}
            />
        </Row>
    }

    let rndDisj = () => {
        <Row alignItems=#"flex-start" spacing=1. style=ReactDOM.Style.make(~marginLeft="7px", ~marginTop="7px", ())>
            {React.string("Disjoints")}
            <MM_cmp_multiline_text
                text=state.disjText
                editMode=state.disjEditMode
                onEditRequested={() => actBeginEdit0(setDisjEditMode)}
                onEditDone={newText => setState(completeDisjEditMode(_,newText))}
            />
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