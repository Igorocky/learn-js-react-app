let beginWorkerInteraction: (
  ~procName: string,
  ~initialRequest: 'req,
  ~onResponse: (~resp: 'resp, ~endWorkerInteraction: unit => unit) => unit,
) => unit
