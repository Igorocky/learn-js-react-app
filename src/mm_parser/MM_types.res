type parserOutput<'a> = {result: 'a, end: int}
type parseResult<'a> = result<parserOutput<'a>, string>

type comment = {text: string, beginIdx: int, endIdx: int}
type nonComment = {text: string, beginIdx: int, endIdx: int, lastComment: option<comment>}
type compressedProof = {labels: array<string>, proof: string}
type sequenceOfSymbols = {
        seqType: string,
        symbols: array<string>,
        uncompressedProof: option<array<string>>,
        compressedProof: option<compressedProof>, 
        beginIdx: int
    }
type labeledSequenceOfSymbols = { label:string, sequence: sequenceOfSymbols, beginIdx: int }
type expression =
    | SequenceOfSymbols(sequenceOfSymbols)
    | LabeledSequenceOfSymbols(labeledSequenceOfSymbols)

exception ParseError(string)