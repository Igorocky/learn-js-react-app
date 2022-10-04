open MM_types
open MM_parserInput

let preprocess: string => parseResult<(array<comment>, array<nonComment>)>
let parseComment: parserInput => parseResult<comment>