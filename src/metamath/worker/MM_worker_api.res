open MM_parser
open MM_context

type mmScope = {
    ast: mmAstNode,
    expectedNumOfAssertions:int,
    stopBefore: option<string>,
    stopAfter: option<string>,
}

type beRequest =
    | ParseMmFile({senderId:string, mmFileText:string})
    | LoadMmContext({senderId:string, scopes:array<mmScope>})

type beResponse =
    | MmFileParseProgress({senderId:string, pct:float})
    | MmFileParsed({senderId:string, parseResult:result<(mmAstNode,array<string>),string>})
    | MmContextLoadProgress({senderId:string, pct:float})
    | MmContextLoaded({senderId:string, ctx:result<mmContext,string>})