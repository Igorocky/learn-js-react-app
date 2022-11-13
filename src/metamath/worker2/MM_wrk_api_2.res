type serialized = Js.Json.t

type workerRequest = {
    senderId: int,
    procName: string,
    body: serialized
}

type workerResponse = {
    senderId: int,
    body: serialized
}

external serialize: 'a => serialized = "%identity"
external deserialize: serialized => 'a = "%identity"