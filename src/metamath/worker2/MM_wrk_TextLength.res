open MM_wrk_client_2

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
        ~initialRequest = MM_wrk_TextLength(CalcLength(text)),
        ~onResponse = (~resp:workerResponseBody,~endWorkerInteraction:unit=>unit) => {
            switch resp {
                | MM_wrk_TextLength(Length(i)) => {
                    endWorkerInteraction()
                    onDone(i)
                }
            }
        }
    )
}