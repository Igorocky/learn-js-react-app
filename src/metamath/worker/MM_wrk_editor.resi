open MM_context
open MM_proof_tree
open MM_syntax_tree
open MM_wrk_settings

type stmtCont =
    | Text(array<string>)
    | Tree(syntaxTreeNode)

let contIsEmpty: stmtCont => bool
let contToArrStr: stmtCont => array<string>
let contToStr: stmtCont => string
let strToCont: string => stmtCont

type userStmtType = [#a | #e | #p]
let userStmtTypeFromStr: string => userStmtType

type userStmt = {
    id: string,

    label: string,
    labelEditMode: bool,
    typ: userStmtType,
    typEditMode: bool,
    cont: stmtCont,
    contEditMode: bool,

    jstfText: string,
    jstfEditMode: bool,

    stmtErr: option<string>,

    expr: option<expr>,
    jstf: option<justification>,
    proof: option<proofTreeNode>,
}

let createEmptyUserStmt: (string, userStmtType, string) => userStmt

type editorState = {
    settingsV:int,
    settings:settings,

    preCtxV: int,
    preCtx: mmContext,

    constsText: string,
    constsEditMode: bool,
    constsErr: option<string>,

    varsText: string,
    varsEditMode: bool,
    varsErr: option<string>,

    disjText: string,
    disjEditMode: bool,
    disjErr: option<string>,
    disj: Belt_MapInt.t<Belt_SetInt.t>,

    wrkCtx: option<(string,mmContext)>,

    nextStmtId: int,
    stmts: array<userStmt>,
    checkedStmtIds: array<string>,
}

let updateStmt: (editorState, string, userStmt => userStmt) => editorState
let isStmtChecked: (editorState, string) => bool
let toggleStmtChecked: (editorState, string) => editorState
let checkAllStmts: editorState => editorState
let uncheckAllStmts: editorState => editorState
let deleteCheckedStmts: editorState => editorState
let canMoveCheckedStmts: (editorState, bool) => bool
let moveCheckedStmts: (editorState, bool) => editorState
let addNewStmt: editorState => editorState
let isSingleStmtChecked: editorState => bool
let duplicateCheckedStmt: editorState => editorState
let canGoEditModeForStmt: (editorState, string) => bool
let setConstsEditMode: editorState => editorState
let completeConstsEditMode: (editorState, string) => editorState
let setVarsEditMode: editorState => editorState
let completeVarsEditMode: (editorState, string) => editorState
let setDisjEditMode: editorState => editorState
let completeDisjEditMode: (editorState, string) => editorState
let setLabelEditMode: (editorState, string) => editorState
let completeLabelEditMode: (editorState, string, Js_string2.t) => editorState
let setContEditMode: (editorState, string) => editorState
let completeContEditMode: (editorState, string, stmtCont) => editorState
let setTypEditMode: (editorState, string) => editorState
let completeTypEditMode: (editorState, string, userStmtType) => editorState
let setJstfEditMode: (editorState, string) => editorState
let completeJstfEditMode: (editorState, string, string) => editorState
let setSettings: (editorState, int, settings) => editorState
let setPreCtx: (editorState, int, mmContext) => editorState
let editorStateHasErrors: editorState => bool
let refreshWrkCtx: editorState => editorState
let prepareProvablesForUnification: editorState => editorState
let validateSyntax: editorState => editorState