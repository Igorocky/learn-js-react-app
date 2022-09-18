type t

let tlFromStrings: array<string> => t
let tlShift: (t, int) => t
let tlConcat: (t, t) => t
let tlConcatAll: array<t> => t
let tlMap: (t, (int, string) => 'a) => array<'a>