open MM_parser

type beRequest =
    | ParseMmFile({senderId:string, mmFileText:string})

type beResponse =
    | MmFileParseProgress({senderId:string, pct:float})
    | MmFileParsed({senderId:string, parseResult:result<(mmAstNode,array<string>),string>})

