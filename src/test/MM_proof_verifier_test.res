open Expln_test
open MM_parser
open MM_context
open MM_proof_verifier

describe("verifyProof", (.) => {
    it("successfully verifies a valid proof", (.) => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/test/resources/demo0.mm")
        let ast = parseMmFile(mmFileText)
        let ctx = createContext(ast, ~stopBefore="th1", ())
        let (exprStr,proof) = traverseAllNodes((), ast, (_,node) => {
            switch node {
                | {stmt:Provable({label:"th1",expr,proof})} => Some((expr,proof))
                | _ => None
            }
        })->Belt.Option.getExn

        //when
        let proof = verifyProof(ctx, makeExpr(ctx, exprStr), proof)

        //then
        assertEq(
            proof->getExprFromNode->ctxExprToStr(ctx, _)->Expln_utils_common.strJoin(~sep=" ", ()),
            "|- t = t"
        )
    })

    it("shows an error for an invalid proof", (.) => {
        //given
        let mmFileText = Expln_utils_files.readStringFromFile("./src/test/resources/demo0.mm")
        let ast = parseMmFile(mmFileText)
        let ctx = createContext(ast, ~stopBefore="th2", ())
        let (exprStr,proof) = traverseAllNodes((), ast, (_,node) => {
            switch node {
                | {stmt:Provable({label:"th2",expr,proof})} => Some((expr,proof))
                | _ => None
            }
        })->Belt.Option.getExn

        //when
        let errorMsg = ref("")
        try {
            let _ = verifyProof(ctx, makeExpr(ctx, exprStr), proof)
        } catch {
            | MmException({msg}) => errorMsg.contents = msg
        }
        assertEq(errorMsg.contents, "!compareExprAfterSubstitution(ess, subs, stack->getExprFromStack(baseIdx+i))")
    })
})