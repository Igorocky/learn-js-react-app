open Belt

type t = list<(int, string)>

let tlFromStrings = ss => ss -> Array.map(s => (0, s)) -> List.fromArray
let tlShift = (t, n) => t -> List.map(((i,s)) => (i+n, s))
let tlConcat = List.concat
let tlConcatAll = List.concatMany
let tlMap = (t, f) => t -> List.map(((i,s)) => f(i, s)) -> List.toArray