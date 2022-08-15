open Js.Console
open Expln_2d

type boundaries = Expln_2d.boundaries
let strF = Belt.Float.toString
let strI = Belt.Int.toString
let viewBox = b => `${strF(b->bndMinX)} ${strF(b->bndMinY)} ${strF(b->bndWidth)} ${strF(b->bndHeight)}`

let svgOnClick = (~mouseEvent, ~viewWidth, ~viewHeight, ~boundaries, ~customHandler) => {
  let target = ref(ReactEvent.Mouse.target(mouseEvent))
  while (target.contents["nodeName"] != "svg") {
    target := target.contents["parentElement"]
  }
  let clientRect = target.contents["getBoundingClientRect"](.)
  let clickViewScreenX = Belt.Int.toFloat(ReactEvent.Mouse.clientX(mouseEvent)) -. clientRect["x"]
  let clickViewScreenY = Belt.Int.toFloat(ReactEvent.Mouse.clientY(mouseEvent)) -. clientRect["y"]
  let h_ = viewHeight
  let w_ = viewWidth
  let h = boundaries->bndHeight
  let w = boundaries->bndWidth
  let pixelSize = if (h_ /. w_ < h /. w) {h /. h_} else {w /. w_}
  let clickViewCenterX = -. w_ /. 2. +. clickViewScreenX
  let clickViewCenterY = -. h_ /. 2. +. clickViewScreenY
  let clickImageCenterX = clickViewCenterX *. pixelSize
  let clickImageCenterY = clickViewCenterY *. pixelSize
  let clickImageX = (boundaries->bndMinX +. boundaries->bndMaxX) /. 2. +. clickImageCenterX
  let clickImageY = (boundaries->bndMinY +. boundaries->bndMaxY) /. 2. +. clickImageCenterY

  let nativeEvent = ReactEvent.Mouse.nativeEvent(mouseEvent)
  customHandler(nativeEvent, clickImageX, clickImageY)
}

let viewWidth = 400.
let viewHeight = 400.
let cellSize = 1.
let boundaries = bndFromPoints([ex->vecBegin, ex->vecBegin->pntTrVec(ex->vecAdd(ey)->vecMult(cellSize *. 8.))])

@react.component
let make = () => {
  let (clicks, setClicks) = React.useState(_ => list{})

  let clickHandler = (e,x,y) => {
    let coords = {"x":x,"y":y}
    log2("clicked: ", coords)
    setClicks(prev => list{coords, ...prev})
  }

  let circles = clicks -> Belt_List.toArray 
    -> Belt_Array.mapWithIndex((i,c) => <circle key=strI(i) cx=strF(c["x"]) cy=strF(c["y"]) r="0.3"/>)
  <svg 
    width=strF(viewWidth) 
    height=strF(viewHeight) 
    viewBox=viewBox(boundaries)
    onMouseDown = {e => svgOnClick(~mouseEvent=e,~boundaries,~viewWidth,~viewHeight,~customHandler=clickHandler)}
  >
   {React.array( circles)}
  </svg>
}
