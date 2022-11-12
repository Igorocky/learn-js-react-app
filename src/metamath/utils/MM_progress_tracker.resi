type progressState
let progressTrackerMake: (~step: float, ~pct: float=?, ~weight: float=?, ~onProgress: float=>unit=?, unit) => progressState
let progressTrackerSetCurrPct: (progressState, float) => progressState
