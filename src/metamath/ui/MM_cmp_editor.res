open MM_context
open Expln_React_common
open Expln_React_Mui
open MM_wrk_FindParens
open Modal
open Expln_utils_promise
open MM_cmp_settings
open MM_id_generator

type userStmtType = Const | Var | Disj | Floating | Essential | Axiom | Provable

type userStmt = {
    id: string,
    typ: userStmtType,
    text: string,
}   

type state = {
    stmtIdGen: idGenState,
    stmts: array<userStmt>,
    focusedStmtId: string,
}

let initialState = {
    {
        stmtIdGen: idGenMake(),
        stmts: [],
        focusedStmtIdx: -1,
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

let addNewStmt = st => {
    {
        ...st,
        stmts: st.
    }
}

@react.component
let make = (~settings:settings, ~ctx:mmContext, ~modalRef:modalRef) => {
    let (state, setState) = React.useState(_ => initialState)

    let rndButtons

    React.string("Editor")
}