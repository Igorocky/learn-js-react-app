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
        Js.Console.log2("ctx", ctx)
        let (exprStr,proof) = traverseAllNodes((), ast, (_,node) => {
            //Js.Console.log2("traverseAllNodes: node = ", node)
            switch node {
                | {stmt:Provable({label:"th1",expr,proof})} => Some((expr,proof))
                | _ => None
            }
        })->Belt.Option.getExn

        //when
        let proof = verifyProof(ctx, makeExpr(ctx, exprStr), proof)
    })
})