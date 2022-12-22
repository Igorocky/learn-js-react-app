let procName: string

type settings = {
    parens: array<int>, 
    nonSyntaxTypes: array<int>
}

type request
type response

let beginWorkerInteractionUsingCtx: (
  ~ctxVer: string,
  ~ctx: MM_context.mmContext,
  ~settingsVer: int,
  ~settings: settings,
  ~procName: string,
  ~initialRequest: 'req,
  ~onResponse: ( ~resp: 'resp, ~sendToWorker: 'req => unit, ~endWorkerInteraction: unit => unit, ) => unit,
) => unit

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
