import {processRequest} from "./MM_worker.bs";

onmessage = e => {
    processRequest(e.data)
}