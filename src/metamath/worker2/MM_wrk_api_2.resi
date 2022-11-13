type serialized

type workerRequest = {
    procName: string, 
    senderId: int, 
    body: serialized
}

type workerResponse = {senderId: int, body: serialized}

let serialize: 'a => serialized

let deserialize: serialized => 'a
