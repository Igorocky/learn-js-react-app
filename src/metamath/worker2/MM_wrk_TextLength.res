open MM_wrk_client_2

let procName = "MM_wrk_TextLength"

type request =
    | CalcLength(string)
type response =
    | Length(int)

let processOnWorkerSide = (~req:request, ~sendToClient:response=>unit):unit => {
    switch req {
        | CalcLength(str) => sendToClient(Length(str->Js_string2.length))
    }
}

let calcTextLength = (~text:string, ~onDone:int=>unit) => {
    beginWorkerInteraction(
        ~initialRequest = CalcLength(text),
        ~onResponse = (~resp:response,~endWorkerInteraction:unit=>unit) => {
            switch resp {
                | Length(i) => {
                    endWorkerInteraction()
                    onDone(i)
                }
            }
        }
    )
}