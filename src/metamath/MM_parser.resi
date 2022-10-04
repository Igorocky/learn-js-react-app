open MM_parserInput

type proof =
    | Uncompressed(array<string>)
    | Compressed(array<string>, string)

type rec stmt =
    | Comment(string)
    | Const(array<string>)
    | Block(array<stmt>)
    | Var(array<string>)
    | Disj(array<string>)
    | Floating(string, string, string)
    | Essential(string, array<string>)
    | Axiom(string, array<string>)
    | Provable(string, array<string>, proof)

type mmAstNode = {
    beginIdx: int,
    endIdx: int,
    stmt: stmt,
}