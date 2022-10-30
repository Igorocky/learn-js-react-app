open Expln_React_common

@module("./AccordionSummaryStyled.js") @react.component
external make: (
    ~expandIcon: reElem=?,
    ~children: reElem,
) => reElem = "default"
