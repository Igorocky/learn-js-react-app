open MM_parenCounter
open MM_context

type contunieInstruction = Continue | Stop

type constParts = {
    length: int,
    begins: array<int>,
    ends: array<int>,
    remainingMinLength: array<int>,
}

type varGroup = {
    leftConstPartIdx:int,
    frmExpr:expr,
    varsBeginIdx:int,
    numOfVars:int,
    mutable exprBeginIdx:int,
    mutable exprEndIdx:int
}

type workVars = {
    numOfCtxVars: int,
    vars: array<int>,
    types: array<int>,
    hypIdxToExprWithWorkVars: array<option<expr>>,
}

type subs = {
    size: int,
    begins: array<int>,
    ends: array<int>,
    exprs: array<expr>,
    isDefined: array<bool>,
    lockedByHypE: array<int>,
}

let createConstParts: expr => constParts
let createMatchingConstParts: constParts => constParts
let createVarGroups: (~frmExpr:expr, ~frmConstParts:constParts) => array<varGroup>
let createSubs: (~numOfVars:int) => subs
let iterateSubstitutions: (
    ~frmExpr:expr, 
    ~expr:expr, 
    ~frmConstParts:constParts, 
    ~constParts:constParts, 
    ~varGroups:array<varGroup>,
    ~subs:subs,
    ~parenCnt:parenCnt,
    ~consumer: subs => contunieInstruction
) => unit

type frameProofDataRec = {
    frame: frame,
    hypsE: array<hypothesis>,
    numOfHypsE:int,
    frmConstParts:array<constParts>,
    constParts:array<constParts>,
    varGroups:array<array<varGroup>>,
    subs:subs,
    workVars:workVars,
}
type frameProofData = array<frameProofDataRec>
let prepareFrameProofData: mmContext => frameProofData
let subsLockDefined: (subs, ~lockLevel:int) => unit
let subsUndefineForLevel: (subs, ~lockLevelToUndefineFrom:int) => unit
let subsUndefineAll: subs => unit
let subsUndefineUnlocked: subs => unit
let applySubs: (~frmExpr:expr, ~subs:subs, ~createWorkVar:int=>int) => expr

//------------------------- TEST ---------------------------

let test_iterateConstParts: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr) => (array<(int,int)>, array<array<(int,int)>>)
let test_iterateSubstitutions: (~ctx:mmContext, ~frmExpr:expr, ~expr:expr) => array<array<expr>>