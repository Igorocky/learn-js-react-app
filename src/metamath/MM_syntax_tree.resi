open MM_context
open MM_proof_table

type rec syntaxTreeNode = {
  parent: option<syntaxTreeNode>,
  label: string,
  children: array<childNode>,
}
and childNode = 
  | Subtree(syntaxTreeNode)
  | Symbol(int)

let buildSyntaxTree: (mmContext, proofTable, int) => syntaxTreeNode
