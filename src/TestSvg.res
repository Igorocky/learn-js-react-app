open Expln_2d
open Expln_utils_common
open Expln_React_Mui

let style = ReactDOM.Style.make

type boundaries = Expln_2d.boundaries
let f2s = Belt.Float.toString
let i2s = Belt.Int.toString
let i2f = Belt_Int.toFloat

let viewWidth = 400.
let viewHeight = 400.
let cellSize = 1.

let margin = cellSize /. 100.
let viewBox = b => `${f2s(b->bndMinX -. margin)} ${f2s(b->bndMinY -. margin)} ${f2s(b->bndWidth +. margin *. 2.)} ${f2s(b->bndHeight +. margin *. 2.)}`
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

let boundaries = bndFromPoints([ex->vecBegin, ex->vecBegin->pntTr(ex->vecAdd(ey)->vecMult(cellSize *. 8.))])

type cell = {x:int, y:int}
let numToCell = n => {
  x: mod(n, 8),
  y: n / 8
}
let cellToName = ({x,y}) => Js_string2.fromCharCode(97+x) ++ i2s(y+1)
let cellToBoudaries = ({x,y}) => {
  let diag = ex
    ->vecMult(cellSize)
    ->vecAdd(ey->vecMult(cellSize))
    ->vecTrDir(ex, i2f(x) *. cellSize)
    ->vecTrDir(ey, i2f(y) *. cellSize)
    bndFromPoints([diag->vecBegin, diag->vecEnd])
}
let allCells = ints(0,63)->Js_array2.map(i => {
  let cell = numToCell(i)
  (cell, cellToBoudaries(cell))
})
let getCellColor = ({x,y}) => mod(x+y, 2)
let getCellColorStr = c => if (getCellColor(c) == 0) {"black"} else {"white"}
let getClickedCellNum = p => allCells->Belt_Array.getIndexBy(((_,b)) => b->bndIncludes(p))
let renderCellByNumOpt = nOpt => switch nOpt {
  | None => React.null
  | Some(n) =>
    let ({x,y}, b) = allCells[n]
    <rect 
      key=`cell-${i2s(x)}-${i2s(y)}` 
      x={b->bndMinX->f2s} 
      y={b->bndMinY->f2s} 
      width={cellSize->f2s}
      height={cellSize->f2s}
      fill=getCellColorStr({x,y})
    />

}

let renderBackgroud = () =>
  <rect key="background" x="-1000" y="-1000" width="10000" height="10000" fill="lightgrey"/>

let renderTransparentPane = () =>
  <rect key="TransparentPane" x="-1000" y="-1000" width="10000" height="10000" fill="black" opacity="0"/>

let renderShape = () => {
  let baseShift = ex->vecMult(cellSize)->vecAdd(ey->vecMult(cellSize))
  let baseToReal = (x,y) =>
    ex
      ->vecMult(Belt_Int.toFloat(x) *. 2. *. cellSize)
      ->vecAdd(ey->vecMult(Belt_Int.toFloat(y) *. 2. *. cellSize))
      ->vecEnd
      ->pntTr(baseShift)
  let basePoints = ints(0,3)->arrFlatMap(y => ints(0,3)->Js_array2.map(x => (x,y)))
  let ps = basePoints 
    -> Js_array2.map(((x,y)) => baseToReal(x,y))
  let points = [ps[0], ps[1], ps[5], ps[6], ps[2], ps[3], ps[7], ps[6], ps[10], ps[11], ps[15], ps[14], ps[10], ps[9], ps[13], ps[12], ps[8], ps[9], ps[5], ps[4], ps[0]]
  let pointsStr = points->Belt_Array.joinWith(" ", p=>`${p->pntX->Belt.Float.toString},${p->pntY->Belt.Float.toString}`)
  <polyline key="shape" stroke="black" fill="none" strokeWidth="0.03" strokeLinejoin="miter" points=pointsStr />
}

let renderShape2 = () => {
  let baseToReal = (x,y) =>
    ex
      ->vecMult(Belt_Int.toFloat(x) *. 2. *. cellSize)
      ->vecAdd(ey->vecMult(Belt_Int.toFloat(y) *. 2. *. cellSize))
      ->vecEnd
  let ps = ints(0,4)->Js_array2.map(x => ints(0,4)->Js_array2.map(y => baseToReal(x,y)))
  let points = [
    ps[0][0], ps[4][0],  ps[4][4],  ps[0][4],   ps[0][0],
    ps[1][0], ps[1][4], ps[2][4], ps[2][0], ps[3][0], ps[3][4],
    ps[4][4], ps[4][3], ps[0][3], ps[0][2], ps[4][2], ps[4][1], ps[0][1]
  ]
  let pointsStr = points->Belt_Array.joinWith(" ", p=>`${p->pntX->Belt.Float.toString},${p->pntY->Belt.Float.toString}`)
  <polyline key="shape" stroke="black" fill="none" strokeWidth="0.03" strokeLinejoin="miter" points=pointsStr />
}
let shape = renderShape2()

let renderCellNameSvg = (n,color) => {
  let (c, _) = allCells[n]
  <text 
    key="cell-name"
    x={ex->vecMult(4. *. cellSize -. cellSize *. 0.5)->vecEnd->pntX->Belt.Float.toString}
    y={ey->vecMult(4. *. cellSize -. cellSize *. 0.5)->vecEnd->pntY->f2s}
    style=style(~fontSize="1", ~fill=color, ())
  >
    {React.string(cellToName(c))}
  </text>
}
let renderCellName = (n,style) => {
  let (c, _) = allCells[n]
  <span style>
    {React.string(cellToName(c))}
  </span>
}

@react.component
let make = () => {
  //let (clicks, setClicks) = React.useState(_ => list{})
  let (clickedCellNum, setClickedCellNum) = React.useState(_ => None)
  let (remainingCellNums, setRemainingCellNums) = React.useState(_ => ints(0,63)->Belt_Array.shuffle)
  let (clickIsCorrect, setClickIsCorrect) = React.useState(_ => true)

  let clickHandler = (e,p) => {
    //let coords = {"x":p->pntX,"y":p->pntY}
    //setClicks(prev => list{coords, ...prev})

    let clickedCellNum = getClickedCellNum(p)
    setClickedCellNum(_ => clickedCellNum)
    let userClickedColor = if (e["button"] == 0) {1} else {0}
    let clickIsCorrect = switch clickedCellNum {
      | None => false
      | Some(n) => n == remainingCellNums[0] && ( userClickedColor == getCellColor(numToCell(n)) || true)
    }
    setClickIsCorrect(_ => clickIsCorrect)
    if (clickIsCorrect) {
      if (Belt_Array.size(remainingCellNums) == 1) {
        setRemainingCellNums(_ => ints(0,63)->Belt_Array.shuffle)
      } else {
        setRemainingCellNums(prev => prev->Belt_Array.slice(~offset=1, ~len=Belt_Array.size(prev)-1))
      }
    }
  }

  //let circles = clicks -> Belt_List.toArray 
    //-> Belt_Array.mapWithIndex((i,c) => <circle key=i2s(i) cx=f2s(c["x"]) cy=f2s(c["y"]) r="0.3"/>)
  let textColor = if(clickIsCorrect){"#017101"}else{"#a30000"}

    <Col alignItems=#center spacing=2. justifyContent=#"space-between" style=ReactDOM.Style.make(~marginTop="100px", ())>
        <svg
          viewBox=viewBox(boundaries)
          width=f2s(viewWidth)
          height=f2s(viewHeight)
          onMouseDown = {e => svgOnClick(~mouseEvent=e,~boundaries,~viewWidth,~viewHeight,~customHandler=clickHandler)}
        >
        {React.array(
          Belt_Array.concatMany([
            [renderBackgroud()],
            //circles,
            [if (clickIsCorrect) {renderCellByNumOpt(clickedCellNum)} else {React.null}],
            [shape],
            [renderCellNameSvg(remainingCellNums[0], textColor)],
            [renderTransparentPane()]
          ])
        )}
        </svg>
    </Col>
}
