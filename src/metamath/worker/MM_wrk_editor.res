open MM_wrk_client
open MM_parser
open MM_context

let procName = "MM_wrk_editor"

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

    settingsV:int,
    ctxV:int,
    prevText: string,

    typ: userStmtType,
    text: option<string>,
    syntaxTree: option<syntaxTreeNode>,
    syntaxError: option<string>,
    
    proof: string,
    proofError: option<string>,
}   

let createEmptyUserStmt = (id, typ) => {
    { id, typ, text: "" }
}

type state = {
    settingsV:int,
    settings:settings,

    constsText: string,
    constsErr:option<string>,
    consts: array<string>,

    varsText: string,
    varsErr: option<string>,
    vars: Belt_MapString.t<string>,

    ctx: mmContext,
    varTypes: Belt_MapString.t<string>,

    nextStmtId: int,
    stmts: array<userStmt>,
    focusedStmtId: string,
}

let initialState = {
    {
        consts: [],
        vars: Belt_MapString.empty,
        ctx: createContext(()),
        varTypes: Belt_MapString.empty,
        nextStmtId: 0,
        stmts: [],
        focusedStmtId: "-1",
    }
}

let updateStmt = (st,id,update) => {
    {
        ...st,
        stmts: st.stmts->Js_array2.map(stmt => if stmt.id == id {update(stmt)} else {stmt})
    }
}

let focusStmt = (st,id) => {
    {
        ...st,
        focusedStmtId: id
    }
}

let addNewStmt = (st, ~beforeId=?, ~afterId=?, ()) => {
    let newId = st.nextStmtId->Belt_Int.toString
    {
        ...st,
        nextStmtId: st.nextStmtId+1,
        stmts: 
            if (beforeId->Belt_Option.isSome || afterId->Belt_Option.isSome) {
                st.stmts->Js_array2.map(stmt => {
                    switch beforeId {
                        | Some(beforeId) if stmt.id == beforeId => [createEmptyUserStmt(newId,Provable), stmt]
                        | _ => {
                            switch afterId {
                                | Some(afterId) if stmt.id == afterId => [stmt, createEmptyUserStmt(newId,Provable)]
                                | _ => [stmt]
                            }
                        }
                    }
                })->Belt_Array.concatMany
            } else {
                st.stmts->Js_array2.concat([createEmptyUserStmt(newId, Provable)])
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
