import {processRequest} from "./MM_wrk_worker_2.bs";

onmessage = e => {
    processRequest(e.data)
}