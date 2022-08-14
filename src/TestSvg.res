
open Js.Console

type boundaries = {minX: float, minY: float, maxX: float, maxY: float}
let bndWidth = b => b.maxX -. b.minX
let bndHeight = b => b.maxY -. b.minY
let viewBox = b => `${Belt.Float.toString(b.minX)} ${Belt.Float.toString(b.minY)} ${Belt.Float.toString(b.maxX -. b.minX)} ${Belt.Float.toString(b.maxY -. b.minY)}`

let svgOnClick = (~mouseEvent, ~boundaries, ~width, ~height, ~customHandler) => {
  let target = ref(ReactEvent.Mouse.target(mouseEvent))
  while (target.contents["nodeName"] != "svg") {
    target := target.contents["parentElement"]
  }
  let clientRect = target.contents["getBoundingClientRect"](.)
  let clickViewScreenX = Belt.Int.toFloat(ReactEvent.Mouse.clientX(mouseEvent)) -. clientRect["x"]
  let clickViewScreenY = Belt.Int.toFloat(ReactEvent.Mouse.clientY(mouseEvent)) -. clientRect["y"]
  let h_ = height
  let w_ = width
  let h = boundaries->bndHeight
  let w = boundaries->bndWidth
  let pixelSize = if (h_ /. w_ < h /. w) {h /. h_} else {w /. w_}
  let clickViewCenterX = -. w_ /. 2. +. clickViewScreenX
  let clickViewCenterY = -. h_ /. 2. +. clickViewScreenY
  let clickImageCenterX = clickViewCenterX *. pixelSize
  let clickImageCenterY = clickViewCenterY *. pixelSize
  let clickImageX = (boundaries.minX +. boundaries.maxX) /. 2. +. clickImageCenterX
  let clickImageY = (boundaries.minY +. boundaries.maxY) /. 2. +. clickImageCenterY

  let nativeEvent = ReactEvent.Mouse.nativeEvent(mouseEvent)
  customHandler(nativeEvent, clickImageX, clickImageY)
}

@react.component
let make = () => {
  let (clicks, setClicks) = React.useState(_ => list{})
  let clickHandler = (e,x,y) => {
    let coords = {"x":x,"y":y}
    log2("clicked: ", coords)
    setClicks(prev => list{coords, ...prev})
  }

  let boundaries = {minX: -10., minY: -10., maxX: 10., maxY: 10.}
  let width = 400.
  let height = 300.
  log2("clicks",    clicks->Belt.List.map(c => <circle cx=Belt.Float.toString(c["x"]) cy=Belt.Float.toString(c["y"]) r="0.3"/>) -> Belt.List.toArray)
  let circles = clicks -> Belt_List.toArray 
    -> Belt_Array.mapWithIndex((i,c) => <circle key=Belt.Int.toString(i) cx=Belt.Float.toString(c["x"]) cy=Belt.Float.toString(c["y"]) r="0.3"/>)
  <svg 
    width=Belt.Float.toString(width) 
    height=Belt.Float.toString(height) 
    viewBox=viewBox(boundaries)
    onMouseDown = {e => svgOnClick(~mouseEvent=e,~boundaries,~width,~height,~customHandler=clickHandler)}
  >
   {React.array( circles)}
  </svg>
}
