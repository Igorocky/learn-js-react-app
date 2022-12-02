let beginWorkerInteraction: (
  ~procName: string,
  ~initialRequest: 'req,
  ~onResponse: (~resp: 'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction: unit => unit) => unit,
) => unit
