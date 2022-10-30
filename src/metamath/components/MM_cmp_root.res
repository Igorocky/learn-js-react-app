open Expln_React_common
open Expln_React_Mui
open MM_parser
open MM_context

@react.component
let make = () => {
    let (rootCtx, setRootCtx) = useState(createEmptyContext())

    <Col>
        <MM_cmp_root_ctx_selector onChange=setRootCtx />
    </Col>
}