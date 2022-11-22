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
    | Text({text:string, syntaxError: option<string>})
    | Tree(syntaxTreeNode)

type userStmtType = [ #e | #a | #p ]

let userStmtTypeFromStr = str => {
    switch str {
        | "e" => #e
        | "a" => #a
        | "p" => #p
        | _ => raise(MmException({msg:`Cannot convert '${str}' to userStmtType`}))
    }
}

type userStmt = {
    id: string,

    // settingsV:int,
    // ctxV:int,
    // prevText: string,

    typ: userStmtType,
    cont: stmtCont,
    
    proof: string,
    proofError: option<string>,
}   

let createEmptyUserStmt = (id, typ) => {
    { id, typ, cont:Text({text:"", syntaxError:None}), proof:"", proofError:None }
}

type state = {
    settingsV:int,
    settings:settings,

    ctxV: int,
    ctx: mmContext,

    constsText: string,
    constsErr:option<string>,
    consts: array<string>,

    varsText: string,
    varsErr: option<string>,
    vars: Belt_MapString.t<string>,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<string>,
}

let initialState = {
    {
        settingsV:-1,
        settings:createDefaultSettings(),

        ctxV: -1,
        ctx: createContext(()),

        constsText: "",
        constsErr:None,
        consts: [],

        varsText: "",
        varsErr: None,
        vars: Belt_MapString.empty,

        nextStmtId: 0,
        stmts: [],
        checkedStmtIds: [],
    }
}

let updateStmt = (st,id,update) => {
    {
        ...st,
        stmts: st.stmts->Js_array2.map(stmt => if stmt.id == id {update(stmt)} else {stmt})
    }
}

let checkStmt = (st,id) => {
    {
        ...st,
        checkedStmtIds: st.checkedStmtIds->Js_array2.concat([id])
    }
}

let addNewStmt = st => {
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

type request = 
    | Req

type response =
    | Resp

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | _ => ()
    }
}
