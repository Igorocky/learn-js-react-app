let beginWorkerInteractionUsingCtx: (
  ~ctxVer: string,
  ~ctx: MM_context.mmContext,
  ~settingsVer: int,
  ~settings: MM_wrk_ctx.settings,
  ~procName: string,
  ~initialRequest: 'req,
  ~onResponse: ( ~resp: 'resp, ~sendToWorker: 'req => unit, ~endWorkerInteraction: unit => unit, ) => unit,
) => unit
