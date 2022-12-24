open Expln_test
open MM_parser
open MM_context
open MM_proof_table
open MM_proof_verifier
open MM_proof_tree
open MM_parenCounter
open MM_substitution
open MM_wrk_editor
open MM_wrk_settings

let createEditorState = (mmFile) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    {
        settingsV: 1,
        settings: {
            parens: "( ) [ ] { }",
            parensIsValid: true,
            nonSyntaxTypes: "|-",
            types: [],
            colors: [],
        },

        preCtxV: 1,
        preCtx: ctx,

        constsText: "",
        constsEditMode: false,
        constsErr: None,

        varsText: "",
        varsEditMode: false,
        varsErr: None,

        disjText: "",
        disjEditMode: false,
        disjErr: None,
        disj: Belt_MapInt.fromArray([]),

        wrkCtx: None,

        nextStmtId: 0,
        stmts: [],
        checkedStmtIds: [],
    }
}

let getVarType = (ctx:mmContext, vName:string) => {
    let varInt = (ctx->makeExprExn([vName]))[0]
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F && hyp.expr[1] == varInt) {
            Some(ctx->ctxIntToStrExn(hyp.expr[0]))
        } else {
            None
        }
    })->Belt_Option.getWithDefault("type-not-found")
}

let demo0 = "./src/metamath/test/resources/demo0.mm"

describe("refreshWrkCtx", _ => {
    it("detects an error when a const is declared twice", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeConstsEditMode(st, "term")

        //when
        let st = refreshWrkCtx(st)

        //then
        assertEq(st.constsErr->Belt_Option.getWithDefault(""), "The symbol 'term' is already used as a constant.")
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("detects an error when a const is declared with the same name as existing variable", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeConstsEditMode(st, "t")

        //when
        let st = refreshWrkCtx(st)

        //then
        assertEq(st.constsErr->Belt_Option.getWithDefault(""), "The symbol 't' is already used as a variable.")
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("creates wrkCtx when only additional constants are defined and there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeConstsEditMode(st, "c1 c2")

        //when
        let st = refreshWrkCtx(st)

        //then
        switch st.wrkCtx {
            | Some((wrkCtxVer,wrkCtx)) => {
                assertEqMsg(wrkCtxVer, "1 1 c1 c2  ", "wrkCtxVer")
                assertEqMsg(wrkCtx->isConst("c1"), true, "c1 is const")
                assertEqMsg(wrkCtx->isConst("c2"), true, "c2 is const")
            }
            | _ => failMsg("A non-empty context was expected")
        }
    })
    
    it("detects an error in variable declaration", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st, "hyp_v1 term v1 \n hyp_v2 term- v2")

        //when
        let st = refreshWrkCtx(st)

        //then
        assertEq(st.constsErr->Belt_Option.isNone, true)
        assertEq(st.varsErr->Belt_Option.getWithDefault(""), "The first symbol in a floating expression must be a constant.")
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("creates wrkCtx when only additional variables are defined and there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st, "hyp_v1 term v1 \n hyp_v2 wff v2")

        //when
        let st = refreshWrkCtx(st)

        //then
        switch st.wrkCtx {
            | Some((wrkCtxVer,wrkCtx)) => {
                assertEqMsg(wrkCtxVer, "1 1  hyp_v1 term v1 \n hyp_v2 wff v2 ", "wrkCtxVer")
                assertEqMsg(wrkCtx->isVar("v1"), true, "v1 is var")
                assertEqMsg(getVarType(wrkCtx, "v1"), "term", "v1 is term")
                assertEqMsg(wrkCtx->isVar("v2"), true, "v2 is var")
                assertEqMsg(getVarType(wrkCtx, "v2"), "wff", "v2 is wff")
            }
            | _ => failMsg("A non-empty context was expected")
        }
    })
    
    
})