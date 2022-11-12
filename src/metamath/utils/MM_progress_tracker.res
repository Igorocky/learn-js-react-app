type progressState = {
    step: float,
    pct: float,
    weight: float,
    onProgress: option<float=>unit>,
    lastSentPct: float,
}

let progressTrackerMake = (~step:float, ~pct=0., ~weight=1., ~onProgress: option<float=>unit>=?, ()) => {
    {
        step,
        pct,
        weight,
        onProgress,
        lastSentPct: -1.
    }
}

let progressTrackerSetCurrPct = (state:progressState, curPct:float):progressState => {
    switch state.onProgress {
        | None => state
        | Some(onProgress) => {
            if (Js.Math.abs_float(state.lastSentPct -. curPct) >= state.step) {
                onProgress(curPct *. state.weight)
                {
                    ...state,
                    lastSentPct: curPct
                }
            } else {
                state
            }
        }
    }
}