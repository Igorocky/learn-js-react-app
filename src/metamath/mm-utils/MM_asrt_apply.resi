open MM_substitution
open MM_context
open MM_parenCounter

type labeledExpr = {
    label:string,
    expr:expr
}

type applyAssertionResult = {
    newVars: array<int>,
    newVarTypes: array<int>,
    newDisj:disjMutable,
    argLabels: array<option<string>>,
    argExprs: array<option<expr>>,
    asrtLabel: string,
    asrtExpr: expr,
}

let applyAssertions: (
    ~maxVar:int,
    ~frms:Belt_MapString.t<frmSubsData>,
    ~nonSyntaxTypes:array<int>,
    ~isDisjInCtx:(int,int)=>bool,
    ~statements:array<labeledExpr>,
    ~result:expr=?,
    ~parenCnt:parenCnt,
    ~frameFilter:frameReduced=>bool=?,
    ~onMatchFound:applyAssertionResult=>contunieInstruction,
    ()
) => unit


// ------ TEST ----------------------

let iterateCombinations: (
    ~numOfStmts:int,
    ~numOfHyps:int,
    ~stmtCanMatchHyp:(int,int)=>bool,
    ~combinationConsumer:array<int>=>contunieInstruction,
) => contunieInstruction