type proof =
    | Uncompressed({labels:array<string>})
    | Compressed({labels:array<string>, compressedProofBlock:string})

type rec mmAstNode = {
    begin: int,
    end: int,
    stmt: stmt
}
and stmt =
    | Comment({text:string})
    | Const({symbols:array<string>})
    | Block({statements:array<mmAstNode>})
    | Var({symbols:array<string>})
    | Disj({vars:array<string>})
    | Floating({label:string, expr:array<string>})
    | Essential({label:string, expr:array<string>})
    | Axiom({label:string, expr:array<string>})
    | Provable({label:string, expr:array<string>, proof:proof})

type mmException = {
    msg:string,
    begin?:int,
}
exception MmException(mmException)

let parseMmFile: string => mmAstNode
let textAt: (string,int) => string