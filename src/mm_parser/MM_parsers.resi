open MM_types
open MM_parserInput

let extractComments: string => (array<comment>, array<nonComment>)
let parseComment: parserInput => parseResult<comment>