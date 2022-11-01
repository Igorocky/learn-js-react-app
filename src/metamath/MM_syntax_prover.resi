open MM_context
open MM_proof_table
open MM_parenCounter

type frameProofData

let prepareFrameProofData: mmContext => frameProofData
let findProof: (~ctx:mmContext, ~frameProofData:frameProofData, ~parenCnt:parenCnt, ~expr:expr, ~proofTbl:proofTable) => int