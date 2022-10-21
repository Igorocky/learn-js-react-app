open MM_parenCounter
open MM_context

type contunieInstruction = Continue | Stop

type subs = {
    size: int,
    begins: array<int>,
    ends: array<int>,
    exprs: array<expr>,
    isDefined: array<bool>,
}

//let createParenCounter: (mmContext,array<(string,string)>) => parentCnt

//let iterateSubs: (~expr:expr, ~frmExpr:expr, ~frame:frame, ~consumer:subs=>contunieInstruction) => unit

//------------------------- TEST ---------------------------

let test_iterateConstParts: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr) => (array<(int,int)>, array<array<(int,int)>>)
let test_iterateSubstitutions: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr) => array<array<expr>>