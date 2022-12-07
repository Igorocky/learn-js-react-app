open MM_context
open MM_proof_table
open MM_parenCounter
open MM_substitution

let findProof: (
    ~frms:array<frmSubsData>, 
    ~parenCnt:parenCnt, 
    ~expr:expr, 
    ~tbl:proofTable,
    ~hyps:array<hypothesis>, 
    ~isDisjInCtx:(int,int) => bool
) => int