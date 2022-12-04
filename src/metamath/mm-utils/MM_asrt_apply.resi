open MM_substitution
open MM_context
open MM_parenCounter

let iterateCombinations: (
    ~numOfStmts:int,
    ~numOfHyps:int,
    ~stmtCanMatchHyp:(int,int)=>bool,
    ~combinationConsumer:array<int>=>contunieInstruction,
) => unit

let applyAssertions: (
    ~frms:frameProofData,
    ~nonSyntaxTypes:array<int>,
    ~statements:array<expr>,
    ~parenCnt:parenCnt,
    ~onMatchFound:frameProofDataRec=>contunieInstruction,
) => unit