open Expln_test
open MM_parser
open MM_context
open MM_syntax_prover
open MM_proof_table
open MM_proof_verifier
open MM_parenCounter
open MM_syntax_tree

type rec syntaxTreeNodeTest = {
    id: string,
    label:string,
    children:array<childNodeTest>,
}
and childNodeTest =
    | Subtree(syntaxTreeNodeTest)
    | Symbol(string)

let rec syntaxTreeToSyntaxTreeTest = (node:syntaxTreeNode) => {
    {
        id: node.id,
        label: node.label,
        children: node.children->Js_array2.map(c => {
            switch c {
                | Subtree(childNode) => Subtree(syntaxTreeToSyntaxTreeTest(childNode))
                | Symbol(s) => Symbol(s)
            }
        })
    }
}


let testSyntaxTree = (~mmFile, ~exprStr, ~expectedSyntaxTree:syntaxTreeNodeTest) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let ast = parseMmFile(mmFileText)
    let ctx = loadContext(ast, ())
    let expr = ctx->makeExpr(exprStr->Js_string2.split(" "))
    let frameProofData = prepareFrameProofData(ctx)
    let parenCnt = parenCntMake(ctx->makeExpr(["(", ")", "{", "}", "[", "]"]))
    let proofTbl = []
    let targetIdx = findProof(~ctx, ~frameProofData, ~parenCnt, ~expr, ~proofTbl)
    //proofTablePrint(ctx, proofTbl, "dddddddddddddd")

    //when
    let actualSyntaxTree = buildSyntaxTree(ctx, proofTbl, targetIdx)

    //then
    //Js.Console.log2("actualSyntaxTree", actualSyntaxTree->syntaxTreeToSyntaxTreeTest->Expln_utils_common.stringify)
    assertEqMsg(actualSyntaxTree->syntaxTreeToSyntaxTreeTest, expectedSyntaxTree, `testSyntaxTree for: ${exprStr}`)
}

describe("buildSyntaxTree", (.) => {
    it("builds correct syntax trees for WWFs", (.) => {
        let demo0 = "./src/test/resources/demo0.mm"
        let setReduced = "./src/test/resources/set-reduced.mm"

        testSyntaxTree(~mmFile=demo0, ~exprStr="wff t = t", 
            ~expectedSyntaxTree = {
                id: "0",
                label: "weq",
                children: [
                    Subtree({
                        id: "1",
                        label: "tt",
                        children: [ 
                            Symbol("t") 
                        ]
                    }),
                    Symbol("=") ,
                    Subtree({
                        id: "2",
                        label: "tt",
                        children: [ 
                            Symbol("t") 
                        ]
                    })
                ]
            }
        )
        //testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( t = t -> t = t ) )", ~expectedProof="( weq wim ) AABZDDCC")
        //testCreateProof(~mmFile=demo0, ~exprStr="wff ( t = t -> ( r = r -> ( t = t -> ( r = r -> t = t ) ) ) )", ~expectedProof="( weq wim ) AACZBBCZEFEDDDD")
        //testCreateProof(~mmFile=demo0, ~exprStr="wff ( ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) -> ( ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) -> ( ( r = r -> t = t ) -> ( r = r -> t = t ) ) ) )", ~expectedProof="( weq wim ) BBCAACDZEDZFDZGD")

        //testCreateProof(~mmFile=setReduced, ~exprStr="wff ph", ~expectedProof="(  ) A")
        //testCreateProof(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )", ~expectedProof="( wb wi wn ) ABCABDBADEDEC")

        //testCreateProof(~mmFile=setReduced, ~exprStr="wff -. ( ( ( ph <-> ps ) -> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) ) -> -. ( -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) -> ( ph <-> ps ) ) )", ~expectedProof="( wb wi wn ) ABCZABDBADEDEZDGFDEDE")
    })
})