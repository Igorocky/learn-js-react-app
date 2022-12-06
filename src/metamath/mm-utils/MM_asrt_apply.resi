open MM_substitution
open MM_context
open MM_parenCounter

type labeledExpr = {
    label:string,
    expr:expr
}

type applyAssertionResult = {
    workVars: array<int>,
    workVarTypes: array<int>,
    disj:disjMutable,
    argLabels: array<option<string>>,
    argExprs: array<option<expr>>,
    asrtLabel: string,
    asrtExpr: expr,
}

let applyAssertions: (
    ~ctx:mmContext,
    ~frms:frameProofData,
//    ~frmsSyntax:frameProofData,
    ~nonSyntaxTypes:array<int>,
    ~statements:array<labeledExpr>,
    ~result:expr=?,
    ~parenCnt:parenCnt,
    ~frameFilter:frame=>bool=?,
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