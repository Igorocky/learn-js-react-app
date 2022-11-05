import {processRequest} from "./MM_backend.bs";


console.log('Hello from worker 2!')

onmessage = e => {
    processRequest(e.data)
}

console.log('Hello from worker 2.1!')