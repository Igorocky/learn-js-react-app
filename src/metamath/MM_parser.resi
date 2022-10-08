type proof =
    | Uncompressed({labels:array<string>})
    | Compressed({labels:array<string>, compressedProofBlock:string})

type rec stmt =
    | Comment({text:string, begin:int, end:int})
    | Const({symbols:array<string>, begin:int, end:int})
    | Block({statements:array<stmt>, begin:int, end:int})
    | Var({symbols:array<string>, begin:int, end:int})
    | Disj({vars:array<string>, begin:int, end:int})
    | Floating({label:string, expr:array<string>, begin:int, end:int})
    | Essential({label:string, expr:array<string>, begin:int, end:int})
    | Axiom({label:string, expr:array<string>, begin:int, end:int})
    | Provable({label:string, expr:array<string>, proof:proof, begin:int, end:int})

let parseMmFile: string => result<stmt,string>