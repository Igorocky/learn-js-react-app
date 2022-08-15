open Js.Console
open Expln_2d
open Expln_common_bindings

type boundaries = Expln_2d.boundaries
let f2s = Belt.Float.toString
let i2s = Belt.Int.toString
let i2f = Belt_Int.toFloat
let viewBox = b => `${f2s(b->bndMinX)} ${f2s(b->bndMinY)} ${f2s(b->bndWidth)} ${f2s(b->bndHeight)}`
let ints = Belt.Array.range

let makePoint = (x,y) => ex->vecMult(x)->vecAdd(ey->vecMult(-.y))->vecEnd

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
  customHandler(nativeEvent, makePoint(clickImageX, clickImageY))
}

let viewWidth = 400.
let viewHeight = 400.
let cellSize = 1.
let boundaries = bndFromPoints([ex->vecBegin, ex->vecBegin->pntTr(ex->vecAdd(ey)->vecMult(cellSize *. 8.))])

type cell = {x:int, y:int}
let numToCell = n => {
  x: mod(n, 8),
  y: n / 8
}
let cellToName = ({x,y}) => Js_string2.fromCharCode(97+x) ++ i2s(y+1)
//ints(0,63)->Belt_Array.forEach(i=>log3(i,numToCell(i), cellToName(numToCell(i))))
let cellToBoudaries = ({x,y}) => {
  let diag = ex
    ->vecMult(cellSize)
    ->vecAdd(ey->vecMult(cellSize))
    ->vecTrDir(ex, i2f(x) *. cellSize)
    ->vecTrDir(ey, i2f(y) *. cellSize)
    bndFromPoints([diag->vecBegin, diag->vecEnd])
}
let allCells = ints(0,63)->arrMap(i => {
  let cell = numToCell(i)
  (cell, cellToBoudaries(cell))
})
let getCellColor = ({x,y}) => mod(x+y, 2)
let getCellColorStr = c => if (getCellColor(c) == 0) {"black"} else {"lightgrey"}
let getClickedCellNum = p => allCells->Belt_Array.getIndexBy(((_,b)) => b->bndIncludes(p))
let renderCellByNumOpt = nOpt => switch nOpt {
  | None => []
  | Some(n) =>
    let ({x,y}, b) = allCells[n]
    [<rect 
      key=`cell-${i2s(x)}-${i2s(y)}` 
      x={b->bndMinX->f2s} 
      y={b->bndMinY->f2s} 
      width={cellSize->f2s}
      height={cellSize->f2s}
      fill="green"
    />]

}

let renderBackgroud = () =>
  <rect key="background" x="-1000" y="-1000" width="10000" height="10000" fill="grey"/>

let renderShape = () => {
  let baseShift = ex->vecMult(cellSize)->vecAdd(ey->vecMult(cellSize))
  let baseToReal = (x,y) =>
    ex
      ->vecMult(Belt_Int.toFloat(x) *. 2. *. cellSize)
      ->vecAdd(ey->vecMult(Belt_Int.toFloat(y) *. 2. *. cellSize))
      ->vecEnd
      ->pntTr(baseShift)
  let basePoints = ints(0,3)->arrFlatMap(y => ints(0,3)->arrMap(x => (x,y)))
  log2("basePoints",basePoints)
  let ps = basePoints 
    -> arrMap(((x,y)) => baseToReal(x,y))
  log2("ps",ps)
  let points = [ps[0], ps[1], ps[5], ps[6], ps[2], ps[3], ps[7], ps[6], ps[10], ps[11], ps[15], ps[14], ps[10], ps[9], ps[13], ps[12], ps[8], ps[9], ps[5], ps[4], ps[0]]
  let pointsStr = points->Belt_Array.joinWith(" ", p=>`${p->pntX->f2s},${p->pntY->f2s}`)
  <polyline key="shape" stroke="black" fill="none" strokeWidth="0.03" strokeLinejoin="miter" points=pointsStr />
}
let shape = renderShape()

@react.component
let make = () => {
  let (clicks, setClicks) = React.useState(_ => list{})
  let (clickedCellNum, setClickedCellNum) = React.useState(_ => None)
  log2("clf2sdCellNum",clickedCellNum)
  let clickHandler = (e,p) => {
    let coords = {"x":p->pntX,"y":p->pntY}
    log2("coords",coords)
    log2("clickedPoint",p)
    setClicks(prev => list{coords, ...prev})
    setClickedCellNum(_ => getClickedCellNum(p))
  }

  let circles = clicks -> Belt_List.toArray 
    -> Belt_Array.mapWithIndex((i,c) => <circle key=i2s(i) cx=f2s(c["x"]) cy=f2s(c["y"]) r="0.3"/>)
  <svg 
    viewBox=viewBox(boundaries)
    width=f2s(viewWidth) 
    height=f2s(viewHeight) 
    onMouseDown = {e => svgOnClick(~mouseEvent=e,~boundaries,~viewWidth,~viewHeight,~customHandler=clickHandler)}
  >
   {React.array(
    Belt_Array.concatMany([
      [renderBackgroud()],
      circles,
      [shape],
      renderCellByNumOpt(clickedCellNum)
    ])
   )}
  </svg>
}
