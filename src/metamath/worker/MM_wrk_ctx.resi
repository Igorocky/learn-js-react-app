let procName: string

type settings = {
    parens: array<int>, 
    nonSyntaxTypes: array<int>
}

type request =
    | CheckVersionsAreUpToDate({ctxVer: string, settingsVer: int})
    | SetCtx({ver: string, ctx: MM_context.mmContext})
    | SetSettings({ver: int, settings: settings})

type response = 
    | GetCtx({ver: string}) 
    | GetSettings({ver: int}) 
    | Ok

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit
