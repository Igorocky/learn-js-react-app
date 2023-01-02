open MM_context
open Expln_React_common
open Expln_React_Mui
open Modal
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_ctx
open MM_wrk_unify
open MM_substitution
open MM_parser
open MM_proof_tree
open Expln_utils_promise

type userStmtLocStor = {
    id: string,

    label: string,
    typ: string,
    cont: string,
    
    jstfText: string,
}   

type editorStateLocStor = {
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
        frms: prepareFrmSubsData(preCtx),

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
        setStatePriv(st => {
            let st = update(st)
            let st = prepareEditorForUnification(st)
            let st = if (st.wrkCtx->Belt_Option.isSome) {
                removeUnusedVars(st)
            } else {
                st
            }
            editorSaveStateToLocStor(st, stateLocStorKey)
            st
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
        state.varsEditMode || state.disjEditMode ||
        state.stmts->Js.Array2.some(stmt => stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode )

    let actAddNewStmt = () => setState(st => {
        let (st, _) = addNewStmt(st)
        st
    })
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

    let actAsrtSearchResultsSelected = selectedResults => {
        setState(st => selectedResults->Js_array2.reduce( addAsrtSearchResult, st ))
    }

    let actWrkSubsSelected = wrkSubs => {
        setState(st => st->applySubstitutionForEditor(wrkSubs))
    }

    let actSearchAsrt = () => {
        switch state.wrkCtx {
            | None => ()
            | Some(wrkCtx) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <MM_cmp_search_asrt
                            modalRef
                            preCtxVer=state.preCtxV
                            preCtx=state.preCtx
                            parenStr=state.settings.parens
                            varsText=state.varsText
                            disjText=state.disjText
                            hyps={
                                state.stmts
                                    ->Js_array2.filter(stmt => stmt.typ == #e)
                                    ->Js_array2.map(stmt => {id:stmt.id, label:stmt.label, text:stmt.cont->contToStr})
                            }
                            wrkCtx
                            frms=state.frms
                            onCanceled={()=>closeModal(modalRef, modalId)}
                            onResultsSelected={selectedResults=>{
                                closeModal(modalRef, modalId)
                                actAsrtSearchResultsSelected(selectedResults)
                            }}
                        />
                    })
                })->ignore
            }
        }
    }

    let actSubstitute = () => {
        switch state.wrkCtx {
            | None => ()
            | Some(wrkCtx) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <MM_cmp_substitution
                            editorState=state
                            wrkCtx
                            onCanceled={()=>closeModal(modalRef, modalId)}
                            onSubstitutionSelected={wrkSubs=>{
                                closeModal(modalRef, modalId)
                                actWrkSubsSelected(wrkSubs)
                            }}
                        />
                    })
                })->ignore
            }
        }
    }

    let actUnifyAllResultsAreReady = proofTreeDto => {
        setState(applyUnifyAllResults(_,proofTreeDto))
    }

    let actUnifyAll = () => {
        switch state.wrkCtx {
            | None => ()
            | Some(wrkCtx) => {
                openModal(modalRef, () => rndProgress(~text="Unifying", ~pct=0.))->promiseMap(modalId => {
                    unify(
                        ~preCtxVer=state.preCtxV,
                        ~preCtx=state.preCtx,
                        ~parenStr=state.settings.parens,
                        ~varsText=state.varsText,
                        ~disjText=state.disjText,
                        ~hyps={
                            state.stmts
                                ->Js_array2.filter(stmt => stmt.typ == #e)
                                ->Js_array2.map(stmt => {id:stmt.id, label:stmt.label, text:stmt.cont->contToStr})
                        },
                        ~stmts={
                            state.stmts
                                ->Js_array2.filter(stmt => stmt.typ == #p)
                                ->Js_array2.map(stmt => {
                                    {
                                        label:Some(stmt.label),
                                        expr:
                                            switch stmt.expr {
                                                | None => raise(MmException({msg:`Expr must be set for all statements before unification.`}))
                                                | Some(expr) => expr
                                            },
                                        justification: stmt.jstf,
                                    }
                                })
                        },
                        ~onProgress = pct => updateModal(modalRef, modalId, () => rndProgress(~text="Unifying", ~pct))
                    )->promiseMap(proofTreeDto => {
                        closeModal(modalRef, modalId)
                        Js.Console.log2("proofTree", proofTreeDto)
                        actUnifyAllResultsAreReady(proofTreeDto)
                    })
                })->ignore
            }
        }
    }

    let rndError = msgOpt => {
        switch msgOpt {
            | None => React.null
            | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
        }
    }

    let rndButtons = () => {
        let generalModificationActionIsEnabled =
            !editIsActive
            && !(mainCheckboxState->Belt_Option.getWithDefault(true))
            && !thereAreSyntaxErrors
            && state.frms->Belt_MapString.size > 0
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
                { rndIconButton(~icon=<Icons2.Search/>, ~onClick=actSearchAsrt, ~active=generalModificationActionIsEnabled ) }
                { rndIconButton(~icon=<Icons2.TextRotationNone/>, ~onClick=actSubstitute, ~active=generalModificationActionIsEnabled ) }
                { rndIconButton(~icon=<Icons2.Hub/>, ~onClick=actUnifyAll, ~active=generalModificationActionIsEnabled ) }
            </Row>
        </Paper>
    }

    let rndStmt = (stmt:userStmt) => {
        <tr key=stmt.id >
            <td>
                <Checkbox
                    disabled=editIsActive
                    checked={state->isStmtChecked(stmt.id)}
                    onChange={_ => actToggleStmtChecked(stmt.id)}
                />
            </td>
            <td>
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
            </td>
        </tr>
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
        <table>
            <tbody>
                { state.stmts->Js_array2.map(rndStmt)->React.array }
            </tbody>
        </table>
    }

    <ContentWithStickyHeader
        top
        header={rndButtons()}
        content={_ => {
            <Col>
                {rndVars()}
                {rndDisj()}
                {rndStmts()}
            </Col>
        }}
    />
}