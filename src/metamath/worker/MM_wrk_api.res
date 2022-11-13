type serialized = Js.Json.t

type workerRequest = {
    procName: string,
    senderId: int,
    body: serialized
}

type workerResponse = {
    senderId: int,
    body: serialized
}

external serialize: 'a => serialized = "%identity"
external deserialize: serialized => 'a = "%identity"