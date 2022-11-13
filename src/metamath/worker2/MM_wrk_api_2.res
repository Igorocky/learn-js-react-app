type workerRequestBody =
    | MM_wrk_TextLength(MM_wrk_TextLength.request)

type workerResponseBody =
    | MM_wrk_TextLength(MM_wrk_TextLength.response)

type workerRequest = {
    senderId: int,
    body: workerRequestBody
}

type workerResponse = {
    senderId: int,
    body: workerResponseBody
}
