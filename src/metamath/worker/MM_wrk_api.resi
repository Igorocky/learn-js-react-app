type serialized

type workerRequest = {
    senderId: int,
    procName: string, 
    body: serialized
}

type workerResponse = {
    senderId: int,
    body: serialized
}

let serialize: 'a => serialized

let deserialize: serialized => 'a
