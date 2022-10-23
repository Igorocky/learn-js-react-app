open MM_context
open MM_substitution
open MM_parser
open MM_syntax_proof_table

let findProof: (~ctx:mmContext, ~expr:expr) => proofTable