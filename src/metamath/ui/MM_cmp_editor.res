open MM_context
open Expln_React_common
open Expln_React_Mui
open MM_wrk_FindParens
open Modal
open Expln_utils_promise
open MM_cmp_settings
open MM_id_generator

// type userStmtType = Const | Var | Disj | Floating | Essential | Axiom | Provable

type userStmt = {
    id: string,
    // typ: userStmtType,
    text: string,
}   

let createEmptyUserStmt = (id/* , typ */) => {
    { id, /* typ,  */text: "" }
}

type state = {
    nextStmtId: int,
    stmts: array<userStmt>,
    focusedStmtId: string,
}

let initialState = {
    {
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
                        | Some(beforeId) if stmt.id == beforeId => [createEmptyUserStmt(newId/* ,Provable */), stmt]
                        | _ => {
                            switch afterId {
                                | Some(afterId) if stmt.id == afterId => [stmt, createEmptyUserStmt(newId/* ,Provable */)]
                                | _ => [stmt]
                            }
                        }
                    }
                })->Belt_Array.concatMany
            } else {
                st.stmts->Js_array2.concat([createEmptyUserStmt(newId/* , Provable */)])
            }
        
    }
}

@react.component
let make = (~settings:settings, ~ctx:mmContext, ~modalRef:modalRef) => {
    let (state, setState) = React.useState(_ => initialState)

    let rndButtons = () => {
        <Row>
            <IconButton key="add-button" onClick={_ => setState(addNewStmt(_, ()))}>
                <Icons2.Add/>
            </IconButton>
        </Row>
    }

    let rndStmts = () => {
        <Col>
            {
                state.stmts->Js_array2.map(stmt => stmt.id->React.string)->React.array
            }
        </Col>
    }

    <Col>
        {rndButtons()}
        {rndStmts()}
    </Col>
}