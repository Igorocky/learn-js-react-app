open MM_types
open MM_parserInput

let extractComments: string => parseResult<(array<comment>, array<nonComment>)>
let parseComment: parserInput => parseResult<comment>