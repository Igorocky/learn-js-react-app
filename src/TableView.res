//open Expln_React_common
//open Expln_React_Mui

type colId = string

// @react.component
// let make = (
//     ~initHeader:array<colId>,
//     ~possibleColumns:array<colId>,
//     ~data:array<'r>, 
//     ~renderHeaderName:colId=>string,
//     ~renderHeaderCell:colId=>reElem,
//     ~renderDataCell:(colId,int)=>reElem,
//     ~onHeaderChange:array<colId>=>unit,
// ) => {
//     let (header, setHeader) = React.useState(_ => initHeader)
//     let (colReorder, setColReorder) = React.useState(_ => false)

//     let headerLen = header->Js_array2.length

//     // let moveCol = (col,right) => {
        
//     // }
// }