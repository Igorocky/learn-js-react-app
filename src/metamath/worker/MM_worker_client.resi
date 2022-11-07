open MM_fe_be

let sendToBe: MM_fe_be.beRequest => unit
let registerBeListener: (beResponse => bool) => int
let unregisterBeListener: int => unit
