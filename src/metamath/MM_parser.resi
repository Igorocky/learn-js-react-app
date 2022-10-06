open MM_parserInput

type proof =
    | Uncompressed(array<string>)
    | Compressed(array<string>, string)

type rec stmt =
    | Comment(int, int, string)
    | Const(int, int, array<string>)
    | Block(int, int, array<stmt>)
    | Var(int, int, array<string>)
    | Disj(int, int, array<string>)
    | Floating(int, int, string, array<string>)
    | Essential(int, int, string, array<string>)
    | Axiom(int, int, string, array<string>)
    | Provable(int, int, string, array<string>, proof)

type mmAstNode = {
    beginIdx: int,
    endIdx: int,
    stmt: stmt,
}

let parseMmFile: string => result<stmt,string>