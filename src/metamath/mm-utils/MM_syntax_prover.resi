open MM_context
open MM_proof_table
open MM_parenCounter
open MM_substitution

let findProof: (~ctx:mmContext, ~frameProofData:frameProofData, ~parenCnt:parenCnt, ~expr:expr, ~proofTbl:proofTable) => int