open MM_context
open MM_parser
open MM_proof_tree
open MM_syntax_tree
open MM_wrk_settings

type stmtCont =
    | Text(array<string>)
    | Tree(syntaxTreeNode)

let contIsEmpty = cont => {
    switch cont {
        | Text(arr) => arr->Js_array2.length == 0
        | Tree(syntaxTreeNode) => syntaxTreeIsEmpty(syntaxTreeNode)
    }
}

let contToArrStr = cont => {
    switch cont {
        | Text(arr) => arr
        | Tree(syntaxTreeNode) => syntaxTreeToSymbols(syntaxTreeNode)
    }
}

let contToStr = cont => {
    cont->contToArrStr->Js_array2.joinWith(" ")
}

let strToCont = str => {
    Text(getSpaceSeparatedValuesAsArray(str))
}

type userStmtType = [ #a | #e | #p ]

let userStmtTypeFromStr = str => {
    switch str {
        | "a" => #a
        | "e" => #e
        | "p" => #p
        | _ => raise(MmException({msg:`Cannot convert '${str}' to userStmtType`}))
    }
}

type userStmt = {
    id: string,

    label: string,
    labelEditMode: bool,
    typ: userStmtType,
    typEditMode: bool,
    cont: stmtCont,
    contErr: option<string>,
    contEditMode: bool,

    jstf: string,
    jstfEditMode: bool,
    jstfErr: option<string>,

    expr: option<expr>,
    proof: option<proofTreeNode>,
}

let createEmptyUserStmt = (id, typ):userStmt => {
    { 
        id, 
        label:"label", labelEditMode:false, 
        typ, typEditMode:false, 
        cont:Text([]), contEditMode:true, contErr:None,
        jstf:"", jstfEditMode:false, jstfErr:None,
        expr:None, proof:None, 
    }
}

type editorState = {
    settingsV:int,
    settings:settings,

    preCtxV: int,
    preCtx: mmContext,

    constsText: string,
    constsEditMode: bool,
    constsErr: option<string>,

    varsText: string,
    varsEditMode: bool,
    varsErr: option<string>,
    vars: array<stmt>,

    disjText: string,
    disjEditMode: bool,
    disjErr: option<string>,
    disj: Belt_MapInt.t<Belt_SetInt.t>,

    wrkCtx: option<(string,mmContext)>,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<string>,
}

let updateStmt = (st:editorState,id,update):editorState => {
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

let checkAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: st.stmts->Js.Array2.map(stmt => stmt.id)
    }
}

let uncheckAllStmts = (st:editorState):editorState => {
    {
        ...st,
        checkedStmtIds: []
    }
}

let deleteCheckedStmts = (st:editorState):editorState => {
    let newStmts = st.stmts->Js_array2.filter(stmt => !isStmtChecked(st,stmt.id))
    let newNextStmtId = if (newStmts->Js_array2.length == 0) { 0 } else { st.nextStmtId }
    {
        ...st,
        stmts: newStmts,
        checkedStmtIds: [],
        nextStmtId: newNextStmtId,
    }
}

let canMoveCheckedStmts = (st:editorState, up):bool => {
    let len = st.stmts->Js_array2.length
    len != 0 && st.checkedStmtIds->Js_array2.length != 0 && (
        (up && !isStmtChecked(st,st.stmts[0].id)) || (!up && !isStmtChecked(st,st.stmts[len-1].id))
    )
}

let moveCheckedStmts = (st:editorState,up):editorState => {
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

let addNewStmt = (st:editorState):editorState => {
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

let canGoEditModeForStmt = (st:editorState,stmtId) => {
    !(st.stmts->Js_array2.some(stmt => 
        stmt.id == stmtId && (stmt.labelEditMode || stmt.typEditMode || stmt.contEditMode || stmt.jstfEditMode)
    ))
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

let setLabelEditMode = (st:editorState, stmtId) => {
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

let setJstfEditMode = (st, stmtId) => {
    if (canGoEditModeForStmt(st, stmtId)) {
        updateStmt(st, stmtId, stmt => {...stmt, jstfEditMode:true})
    } else {
        st
    }
}

let completeJstfEditMode = (st, stmtId, newJstf) => {
    updateStmt(st, stmtId, stmt => {
        {
            ...stmt,
            jstf:newJstf,
            jstfEditMode: false
        }
    })
}

let setSettings = (st, settingsV, settings) => {
    { ...st, settingsV, settings }
}

let setPreCtx = (st, preCtxV, preCtx) => {
    { ...st, preCtxV, preCtx }
}

let stableSortStmts = (st, comp: (userStmt,userStmt)=>int) => {
    let stmtsLen = st.stmts->Js.Array2.length
    if (stmtsLen < 2) {
        st
    } else {
        let newStmts = st.stmts->Js.Array2.copy
        let changed = ref(true)
        let e = ref(stmtsLen - 2)
        while (e.contents >= 1 && changed.contents) {
            changed.contents = false
            for i in 0 to e.contents {
                if (comp(newStmts[i], newStmts[i+1]) > 0) {
                    let tmp = newStmts[i]
                    newStmts[i] = newStmts[i+1]
                    newStmts[i+1] = tmp
                    changed.contents = true
                }
            }
            e.contents = e.contents - 1
        }
        {
            ...st,
            stmts: newStmts
        }
    }
}

let sortStmtsByType = st => {
    let stmtToInt = stmt => {
        switch stmt.typ {
            | #a => 1
            | #e => 2
            | #p => 3
        }
    }
    st->stableSortStmts((a,b) => stmtToInt(a) - stmtToInt(b))
}

let unify = st => {
    let st = sortStmtsByType(st)
    st
}

let removeAllErrorsInUserStmt = stmt => {
    {
        ...stmt,
        contErr: None,
        jstfErr: None,
    }
}

let removeAllErrorsInEditorState = st => {
    {
        ...st,
        constsErr: None,
        varsErr: None,
        disjErr: None,
        stmts: st.stmts->Js_array2.map(removeAllErrorsInUserStmt)
    }
}

let editorStateHasErrors = st => {
    st.constsErr->Belt_Option.isSome ||
        st.varsErr->Belt_Option.isSome ||
        st.disjErr->Belt_Option.isSome ||
        st.stmts->Js_array2.some(stmt => {
            stmt.contErr->Belt_Option.isSome ||
                stmt.jstfErr->Belt_Option.isSome
        })
}

let refreshWrkCtx = (st:editorState):editorState => {
    let actualWrkCtxVer = [
        st.settingsV->Belt_Int.toString,
        st.preCtxV->Belt_Int.toString,
        st.constsText,
        st.varsText,
        st.disjText,
    ]->Js.Array2.joinWith(" ")
    let mustUpdate = switch st.wrkCtx {
        | None => true
        | Some((existingWrkCtxVer,_)) if existingWrkCtxVer != actualWrkCtxVer => true
        | _ => false
    }
    if (!mustUpdate) {
        st
    } else {
        let st = removeAllErrorsInEditorState(st)
        let wrkCtx = st.preCtx->cloneContext
        let constsArr = getSpaceSeparatedValuesAsArray(st.constsText)
        let st = if (constsArr->Js.Array2.length == 0) {
            {...st, constsErr:None}
        } else {
            try {
                constsArr->Js_array2.forEach(wrkCtx->addConstToRoot)
                {...st, constsErr:None}
            } catch {
                | MmException({msg}) => {...st, constsErr:Some(msg)}
            }
        }
        if (editorStateHasErrors(st)) {
            {...st, wrkCtx:None}
        } else {
            {...st, wrkCtx:Some((actualWrkCtxVer, wrkCtx))}
        }
    }
}