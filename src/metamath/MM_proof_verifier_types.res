type expr = array<int>

type mandHyp =
    | F(expr)
    | E(expr)

type frame = {
    label: string,
    disj: Belt.Map.Int.t<int>,
    hyps: array<mandHyp>,
    asrt: expr
}
