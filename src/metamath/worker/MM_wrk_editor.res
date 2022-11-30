open MM_wrk_client
open MM_parser
open MM_context
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

let procName = "MM_wrk_editor"

type stmtCont =
    | Text({text:array<string>, syntaxError: option<string>})
    | Tree(syntaxTreeNode)

let contIsEmpty = cont => {
    switch cont {
        | Text({text}) => text->Js.Array2.length == 0
        | Tree(syntaxTreeNode) => syntaxTreeIsEmpty(syntaxTreeNode)
    }
}

let contToArrStr = cont => {
    switch cont {
        | Text({text}) => text
        | Tree(syntaxTreeNode) => syntaxTreeToSymbols(syntaxTreeNode)
    }
}

let contToStr = cont => {
    cont->contToArrStr->Js_array2.joinWith(" ")
}

let strToCont = str => {
    Text({
        text: getSpaceSeparatedValuesAsArray(str),
        syntaxError: None
    })
}

type userStmtType = [ #e | #a | #p ]

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
    
    proof: string,
    proofEditMode: bool,
    proofError: option<string>,
}

type userStmtLocStor = {
    id: string,

    label: string,
    typ: string,
    cont: string,
    
    proof: string,
}   

let createEmptyUserStmt = (id, typ) => {
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

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<string>,
}

type stateLocStor = {
    constsText: string,

    varsText: string,

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
                    constsText: d->str("constsText"),
                    varsText: d->str("varsText"),
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

type request = 
    | Req

type response =
    | Resp

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | _ => ()
    }
}
