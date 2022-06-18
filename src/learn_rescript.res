let {log, log2} = module(Js)
//let log = Js.log
//let log2 = Js.log2
let {make:toStr} = module(Js.String2)

let f = (~x=100, ~y, ()) => x+y 
let f2 = f(~y=5, ())
log2("f2 = ", f2)

let f3 = a => a + 7
log2("f3(...) =", f3(22))

let g = _ => {
    3
}