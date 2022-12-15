open MM_context
open MM_parenCounter
open MM_substitution
open MM_proof_table

type rootStmt = {
    label: string, 
    expr: expr, 
    justification: option<array<string>>
}

type rec proofTreeNode = {
  expr: expr,
  mutable parents: option<array<exprSource>>,
  children: array<proofTreeNode>,
  mutable proof: option<exprSource>,
  mutable dist: int,
  mutable syntax: option<proofTreeNode>,
}
and exprSource =
  | VarType
  | Hypothesis({label: string})
  | Assertion({args: array<proofTreeNode>, label: string})

type proofTree = {
  parenCnt: parenCnt,
  frms: Belt_MapString.t<frmSubsData>,
  hyps: Belt_MutableMap.t<expr, hypothesis, ExprCmp.identity>,
  newVars: Belt_MutableSet.t<expr, ExprCmp.identity>,
  newVarTypes: Belt_MutableMapInt.t<int>,
  mutable maxVar: int,
  disj: disjMutable,
  nodes: Belt_MutableMap.t<expr, proofTreeNode, ExprCmp.identity>,
}

let proofTreeProve: (
  ~parenCnt: parenCnt,
  ~frms: Belt_MapString.t<frmSubsData>,
  ~hyps: Belt_MapString.t<hypothesis>,
  ~maxVar: int,
  ~disj: disjMutable,
  ~stmts: array<rootStmt>,
  ~searchDepth: int,
) => proofTree

let proofTreeCreateProofTable: proofTreeNode => proofTable
