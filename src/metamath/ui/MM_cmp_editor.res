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
open MM_wrk_editor


@react.component
let make = (~modalRef:modalRef, ~settings:settings, ~settingsV:int, ~ctx:mmContext, ~ctxV:int) => {
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