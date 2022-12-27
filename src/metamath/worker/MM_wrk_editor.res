open MM_context
open MM_parser
open MM_proof_tree
open MM_syntax_tree
open MM_wrk_settings
open MM_asrt_apply
open MM_parenCounter
open MM_substitution

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
    contEditMode: bool,
    
    jstfText: string,
    jstfEditMode: bool,

    stmtErr: option<string>,

    expr: option<expr>,
    jstf: option<justification>,
    proof: option<proofTreeNode>,
}

let createEmptyUserStmt = (id, typ, label):userStmt => {
    { 
        id, 
        label, labelEditMode:false, 
        typ, typEditMode:false, 
        cont:Text([]), contEditMode:true,
        jstfText:"", jstfEditMode:false,
        stmtErr: None,
        expr:None, jstf:None, proof:None, 
    }
}

type wrkPrecalcData = {
    ver: string,
    wrkCtx: mmContext,
    parens: array<int>,
    parenCnt: parenCnt,
    frms: Belt_MapString.t<frmSubsData>,
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

    disjText: string,
    disjEditMode: bool,
    disjErr: option<string>,
    disj: Belt_MapInt.t<Belt_SetInt.t>,

    wrkPreData: option<wrkPrecalcData>,

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

let createNewLabel = (st:editorState, prefix:string):string => {
    let isLabelDefinedInCtx = label => {
        switch st.wrkPreData {
            | Some({wrkCtx}) => wrkCtx->isHyp(label) || wrkCtx->isAsrt(label)
            | None => false
        }
    }
    
    let usedLabels = st.stmts->Js_array2.map(stmt=>stmt.label)
    let i = ref(1)
    let newLabel = ref(prefix ++ i.contents->Belt_Int.toString)
    while (usedLabels->Js.Array2.includes(newLabel.contents) || isLabelDefinedInCtx(newLabel.contents)) {
        i.contents = i.contents + 1
        newLabel.contents = prefix ++ i.contents->Belt_Int.toString
    }
    newLabel.contents
}

let addNewStmt = (st:editorState):(editorState,string) => {
    let newId = st.nextStmtId->Belt_Int.toString
    let newLabel = createNewLabel(st, "stmt")
    let idToAddBefore = st.stmts->Js_array2.find(stmt => st.checkedStmtIds->Js_array2.includes(stmt.id))->Belt_Option.map(stmt => stmt.id)
    (
        {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                switch idToAddBefore {
                    | Some(idToAddBefore) => {
                        st.stmts->Js_array2.map(stmt => {
                            if (stmt.id == idToAddBefore) {
                                [createEmptyUserStmt(newId,#p,newLabel), stmt]
                            } else {
                                [stmt]
                            }
                        })->Belt_Array.concatMany
                    }
                    | None => st.stmts->Js_array2.concat([createEmptyUserStmt(newId, #p, newLabel)])
                }
        },
        newId
    )
}

let isSingleStmtChecked = st => st.checkedStmtIds->Js_array2.length == 1

let duplicateCheckedStmt = st => {
    if (!isSingleStmtChecked(st)) {
        st
    } else {
        let newId = st.nextStmtId->Belt_Int.toString
        let newLabel = createNewLabel(st, "stmt")
        let idToAddAfter = st.checkedStmtIds[0]
        {
            ...st,
            nextStmtId: st.nextStmtId+1,
            stmts: 
                st.stmts->Js_array2.map(stmt => {
                    if (stmt.id == idToAddAfter) {
                        [stmt, {...stmt, id:newId, label:newLabel}]
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
            jstfText:newJstf,
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
        while (e.contents >= 0 && changed.contents) {
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

let removeAllErrorsInUserStmt = stmt => {
    {
        ...stmt,
        stmtErr: None,
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

let userStmtHasErrors = stmt => {
    stmt.stmtErr->Belt_Option.isSome
}

let editorStateHasErrors = st => {
    st.constsErr->Belt_Option.isSome ||
        st.varsErr->Belt_Option.isSome ||
        st.disjErr->Belt_Option.isSome ||
        st.stmts->Js_array2.some(userStmtHasErrors)
}

let parseConstants = (st,wrkCtx) => {
    if (editorStateHasErrors(st)) {
        st
    } else {
        let constsArr = getSpaceSeparatedValuesAsArray(st.constsText)
        if (constsArr->Js.Array2.length == 0) {
            st
        } else {
            try {
                constsArr->Js_array2.forEach(wrkCtx->addLocalConst)
                st
            } catch {
                | MmException({msg}) => {...st, constsErr:Some(msg)}
            }
        }
    }
}

let addVarFromString = (str,wrkCtx) => {
    let arr = getSpaceSeparatedValuesAsArray(str)
    if (arr->Js_array2.length != 3) {
        raise(MmException({msg:`Cannot convert '${str}' to Var statement.`}))
    } else {
        wrkCtx->applySingleStmt(Var({symbols:[arr[2]]}))
        wrkCtx->applySingleStmt(Floating({label:arr[0], expr:[arr[1], arr[2]]}))
    }
}

let newLineRegex = %re("/[\n\r]/")
let parseVariables = (st,wrkCtx) => {
    if (editorStateHasErrors(st)) {
        st
    } else {
        let varLines = st.varsText
            ->Js_string2.splitByRe(newLineRegex)
            ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
            ->Js_array2.filter(s => s->Js_string2.length > 0)
        if (varLines->Js.Array2.length == 0) {
            st
        } else {
            try {
                varLines->Js_array2.forEach(addVarFromString(_,wrkCtx))
                st
            } catch {
                | MmException({msg}) => {...st, varsErr:Some(msg)}
            }
        }
    }
}

let addDisjFromString = (str,wrkCtx) => {
    wrkCtx->applySingleStmt(Disj({vars:getSpaceSeparatedValuesAsArray(str)}))
}

let parseDisjoints = (st,wrkCtx) => {
    if (editorStateHasErrors(st)) {
        st
    } else {
        let disjLines = st.disjText
            ->Js_string2.splitByRe(newLineRegex)
            ->Js_array2.map(so => so->Belt_Option.getWithDefault("")->Js_string2.trim)
            ->Js_array2.filter(s => s->Js_string2.length > 0)
        if (disjLines->Js.Array2.length == 0) {
            st
        } else {
            try {
                disjLines->Js_array2.forEach(addDisjFromString(_,wrkCtx))
                st
            } catch {
                | MmException({msg}) => {...st, disjErr:Some(msg)}
            }
        }
    }
}

let addStmtToCtx = (stmt:userStmt, wrkCtx:mmContext):userStmt => {
    if (stmt.typ != #a && stmt.typ != #e) {
        raise(MmException({msg:`Cannot put a statement of type '${stmt.typ :> string}' to mm ctx.`}))
    }
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        try {
            if (stmt.typ == #a) {
                wrkCtx->applySingleStmt(Axiom({label:stmt.label, expr:stmt.cont->contToArrStr}))
            } else if (stmt.typ == #e) {
                wrkCtx->applySingleStmt(Essential({label:stmt.label, expr:stmt.cont->contToArrStr}))
            }
            stmt
        } catch {
            | MmException({msg}) => {...stmt, stmtErr:Some(msg)}
        }
    }
}

let createParensInt = (st,wrkCtx):array<int> => {
    let parensStr = st.settings.parens->getSpaceSeparatedValuesAsArray
    let parensInt = []
    let maxI = parensStr->Js_array2.length / 2 - 1
    for i in 0 to maxI {
        let leftParen = parensStr[i*2]
        let rightParen = parensStr[i*2+1]
        switch wrkCtx->ctxSymToInt(leftParen) {
            | Some(leftParenInt) if wrkCtx->isConst(leftParen) => {
                switch wrkCtx->ctxSymToInt(rightParen) {
                    | Some(rightParenInt) if wrkCtx->isConst(rightParen) => {
                        parensInt->Js.Array2.push(leftParenInt)->ignore
                        parensInt->Js.Array2.push(rightParenInt)->ignore
                    }
                    | _ => ()
                }
            }
            | _ => ()
        }
    }
    parensInt
}

let refreshWrkPreData = (st:editorState):editorState => {
    let st = sortStmtsByType(st)
    let actualWrkCtxVer = [
        st.settingsV->Belt_Int.toString,
        st.preCtxV->Belt_Int.toString,
        st.constsText,
        st.varsText,
        st.disjText,
        st.stmts->Js_array2.reduce(
            (acc,stmt) => {
                acc ++ if (stmt.typ == #a || stmt.typ == #e) {
                    "::: " ++ stmt.label ++ " " ++ stmt.cont->contToArrStr->Js_array2.joinWith(" ") ++ " :::"
                } else {
                    ""
                }
            },
            ""
        )
    ]->Js.Array2.filter(str => str->Js.String2.trim->Js_string2.length != 0)->Js.Array2.joinWith(" ")
    let mustUpdate = switch st.wrkPreData {
        | None => true
        | Some({ver:existingWrkCtxVer}) if existingWrkCtxVer != actualWrkCtxVer => true
        | _ => false
    }
    if (!mustUpdate) {
        st
    } else {
        let st = removeAllErrorsInEditorState(st)
        let wrkCtx = createContext(~parent=st.preCtx, ())
        let st = parseConstants(st,wrkCtx)
        let st = parseVariables(st,wrkCtx)
        let st = parseDisjoints(st,wrkCtx)
        let st = st.stmts->Js_array2.reduce(
            (st,stmt) => {
                if (editorStateHasErrors(st) || stmt.typ == #p) {
                    st
                } else {
                    st->updateStmt(stmt.id, _ => addStmtToCtx(stmt,wrkCtx))
                }
            },
            st
        )
        if (editorStateHasErrors(st)) {
            {...st, wrkPreData:None}
        } else {
            let parensInt = createParensInt(st, wrkCtx)
            {
                ...st, 
                wrkPreData: Some(
                    {
                        ver:actualWrkCtxVer, 
                        wrkCtx, 
                        parens: parensInt,
                        parenCnt: parenCntMake(parensInt),
                        frms: prepareFrmSubsData(wrkCtx),
                    }
                )
            }
        }
    }
}

let parseJstf = jstfText => {
    let jstfTrim = jstfText->Js_string2.trim
    if (jstfTrim->Js_string2.length == 0) {
        None
    } else {
        let argsAndAsrt = jstfTrim->Js_string2.split(":")
        if (argsAndAsrt->Js_array2.length != 2) {
            raise(MmException({msg:`Cannot parse justification: '${jstfText}' [1].`}))
        }
        Some({
            args: argsAndAsrt[0]->getSpaceSeparatedValuesAsArray,
            asrt: argsAndAsrt[1]->Js_string2.trim
        })
    }
}

let setExprAndJstf = (stmt:userStmt,wrkCtx:mmContext):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        try {
            {
                ...stmt,
                expr: Some(wrkCtx->ctxSymsToIntsExn(stmt.cont->contToArrStr)),
                jstf: parseJstf(stmt.jstfText)
            }
        } catch {
            | MmException({msg}) => {...stmt, stmtErr:Some(msg)}
        }
    }
}

let isLabelDefined = (label:string, wrkCtx:mmContext, usedLabels:Belt_MutableSetString.t) => {
    usedLabels->Belt_MutableSetString.has(label) || wrkCtx->isHyp(label) || wrkCtx->isAsrt(label)
}

let validateJstfRefs = (stmt:userStmt, wrkCtx:mmContext, usedLabels:Belt_MutableSetString.t):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        switch stmt.jstf {
            | None => stmt
            | Some({args,asrt}) => {
                switch args->Js_array2.find(ref => !isLabelDefined(ref,wrkCtx,usedLabels)) {
                    | Some(ref) => {
                        {...stmt, stmtErr:Some(`The reference '${ref}' is not defined.`)}
                    }
                    | None => {
                        if (!(wrkCtx->isAsrt(asrt))) {
                            {...stmt, stmtErr:Some(`The label '${asrt}' doesn't refer to any assertion.`)}
                        } else {
                            stmt
                        }
                    }
                }
            }
        }
    }
}

let validateStmtLabel = (stmt:userStmt, wrkCtx:mmContext, usedLabels:Belt_MutableSetString.t):userStmt => {
    if (userStmtHasErrors(stmt)) {
        stmt
    } else {
        if (isLabelDefined(stmt.label,wrkCtx,usedLabels)) {
            {...stmt, stmtErr:Some(`Cannot reuse label '${stmt.label}'.`)}
        } else {
            stmt
        }
    }
}

let prepareProvablesForUnification = (st:editorState):editorState => {
    switch st.wrkPreData {
        | None => st
        | Some(preData) => {
            let usedLabels = Belt_MutableSetString.make()
            st.stmts->Js_array2.reduce(
                (st,stmt) => {
                    if (editorStateHasErrors(st) || stmt.typ != #p) {
                        st
                    } else {
                        let stmt = setExprAndJstf(stmt, preData.wrkCtx)
                        let stmt = validateJstfRefs(stmt, preData.wrkCtx, usedLabels)
                        let stmt = validateStmtLabel(stmt, preData.wrkCtx, usedLabels)
                        usedLabels->Belt_MutableSetString.add(stmt.label)
                        st->updateStmt(stmt.id, _ => stmt)
                    }
                },
                st
            )
        }
    }
}

let validateSyntax = st => {
    let st = removeAllErrorsInEditorState(st)
    let st = refreshWrkPreData(st)
    let st = prepareProvablesForUnification(st)
    st
}

let createNewVars = (st:editorState, varTypes:array<int>):(editorState,array<int>) => {
    switch st.wrkPreData {
        | None => raise(MmException({msg:`Cannot create new variables without wrkPreData.`}))
        | Some({wrkCtx}) => {
            let numOfVars = varTypes->Js_array2.length
            if (numOfVars == 0) {
                (st,[])
            } else {
                let newVarNames = wrkCtx->generateNewVarNames(varTypes)
                let newHypLabels = wrkCtx->generateNewLabels(~prefix="var", ~amount=numOfVars)
                wrkCtx->applySingleStmt(Var({symbols:newVarNames}))
                let varTypeNames = wrkCtx->ctxIntsToSymsExn(varTypes)
                newHypLabels->Js.Array2.forEachi((label,i) => {
                    wrkCtx->applySingleStmt(Floating({label, expr:[varTypeNames[i], newVarNames[i]]}))
                })
                let newVarInts = wrkCtx->ctxSymsToIntsExn(newVarNames)
                let newVarsText = newHypLabels->Js.Array2.mapi((label,i) => {
                    `${label} ${varTypeNames[i]} ${newVarNames[i]}`
                })->Js_array2.joinWith("\n")
                (
                    {
                        ...st,
                        varsText: st.varsText ++ (if (st.varsText->Js.String2.length != 0) {"\n"} else {""}) ++ newVarsText
                    },
                    newVarInts
                )
            }
            
        }
    }
}

let createNewDisj = (st:editorState, newDisj:disjMutable):editorState => {
    switch st.wrkPreData {
        | None => raise(MmException({msg:`Cannot create new disjoints without wrkPreData.`}))
        | Some({wrkCtx}) => {
            newDisj->disjForEachArr(varInts => {
                wrkCtx->applySingleStmt(Disj({vars:wrkCtx->ctxIntsToSymsExn(varInts)}))
            })
            let newDisjTextLines = []
            newDisj->disjForEachArr(varInts => {
                newDisjTextLines->Js.Array2.push(wrkCtx->ctxIntsToSymsExn(varInts))->ignore
            })
            if (newDisjTextLines->Js.Array2.length == 0) {
                st
            } else {
                let newDisjText = newDisjTextLines->Js.Array2.joinWith("\n")
                {
                    ...st,
                    disjText: st.disjText ++ "\n" ++ newDisjText
                }
            }
        }
    }
}

let addAsrtSearchResult = (st:editorState, applRes:applyAssertionResult):editorState => {
    switch st.wrkPreData {
        | None => raise(MmException({msg:`Cannot add assertion search result without wrkPreData.`}))
        | Some({wrkCtx, frms}) => {
            switch frms->Belt_MapString.get(applRes.asrtLabel) {
                | None => raise(MmException({msg:`Cannot find assertion with label '${applRes.asrtLabel}'.`}))
                | Some(frm) => {
                    let (st, newCtxVarInts) = createNewVars(st,applRes.newVarTypes)
                    let applResVarToCtxVar = Belt_MutableMapInt.make()
                    applRes.newVars->Js.Array2.forEachi((applResVarInt,i) => {
                        applResVarToCtxVar->Belt_MutableMapInt.set(applResVarInt, newCtxVarInts[i])
                    })
                    let newCtxDisj = disjMutableMake()
                    applRes.newDisj->disjForEach((n,m) => {
                        let newN = switch applResVarToCtxVar->Belt_MutableMapInt.get(n) {
                            | None => n
                            | Some(newN) => newN
                        }
                        let newM = switch applResVarToCtxVar->Belt_MutableMapInt.get(m) {
                            | None => m
                            | Some(newM) => newM
                        }
                        newCtxDisj->addDisjPairToMap(newN, newM)
                    })
                    let st = createNewDisj(st, newCtxDisj)
                    let selectionWasEmpty = st.checkedStmtIds->Js.Array2.length == 0
                    let st = if (!selectionWasEmpty || st.stmts->Js.Array2.length == 0) {
                        st
                    } else {
                        st->toggleStmtChecked(st.stmts[0].id)
                    }
                    let mainStmtLabel = createNewLabel(st, "stmt")
                    let argLabels = []
                    let stMut = ref(st)
                    frm.frame.hyps->Js_array2.forEach(hyp => {
                        if (hyp.typ == E) {
                            let argLabel = createNewLabel(stMut.contents, mainStmtLabel ++ "-" ++ hyp.label)
                            argLabels->Js.Array2.push(argLabel)->ignore
                            let argExprText = applySubs(
                                ~frmExpr=hyp.expr,
                                ~subs=applRes.subs,
                                ~createWorkVar=_=>raise(MmException({msg:`Cannot create a work variable in addAsrtSearchResult [1].`}))
                            )
                                ->Js.Array2.map(appResInt => {
                                    switch applResVarToCtxVar->Belt_MutableMapInt.get(appResInt) {
                                        | None => appResInt
                                        | Some(ctxVar) => ctxVar
                                    }
                                })
                                ->ctxIntsToStrExn(wrkCtx, _)
                            let (st, newStmtId) = addNewStmt(stMut.contents)
                            stMut.contents = st
                            stMut.contents = updateStmt(stMut.contents, newStmtId, stmt => {
                                {
                                    ...stmt,
                                    typ: #p,
                                    label: argLabel,
                                    cont: strToCont(argExprText),
                                    contEditMode: false,
                                }
                            })
                        }
                    })
                    let st = stMut.contents
                    let asrtExprText = applySubs(
                        ~frmExpr=frm.frame.asrt,
                        ~subs=applRes.subs,
                        ~createWorkVar=_=>raise(MmException({msg:`Cannot create a work variable in addAsrtSearchResult [2].`}))
                    )
                        ->Js.Array2.map(appResInt => {
                            switch applResVarToCtxVar->Belt_MutableMapInt.get(appResInt) {
                                | None => appResInt
                                | Some(ctxVar) => ctxVar
                            }
                        })
                        ->ctxIntsToStrExn(wrkCtx, _)
                    let (st, newStmtId) = addNewStmt(st)
                    let jstfText = argLabels->Js.Array2.joinWith(" ") ++ ": " ++ applRes.asrtLabel
                    let st = updateStmt(st, newStmtId, stmt => {
                        {
                            ...stmt,
                            typ: #p,
                            label: mainStmtLabel,
                            cont: strToCont(asrtExprText),
                            contEditMode: false,
                            jstfText,
                        }
                    })
                    if (selectionWasEmpty) {
                        st->uncheckAllStmts
                    } else {
                        st
                    }
                }
            }
        }
    }
}