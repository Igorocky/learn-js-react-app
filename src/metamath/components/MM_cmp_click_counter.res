open Expln_React_common
open Expln_React_Mui
open MM_parser
open MM_context


@react.component
let make = (~title) => {
    let (cnt, setCnt) = useStateF(_=>0)

    <Button onClick={_=>setCnt(prev => prev+1)}>
        {React.string(`${title}. Clicked: ${cnt->Belt_Int.toString}`)}
    </Button>
}