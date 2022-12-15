open Expln_test
open MM_parser
open MM_context
open MM_syntax_prover
open MM_parenCounter
open MM_syntax_tree
open MM_substitution

type rec syntaxTreeNodeTest = {
    label:string,
    children:array<childNodeTest>,
}
and childNodeTest =
    | Subtree(syntaxTreeNodeTest)
    | Symbol(string)

let rec syntaxTreeToSyntaxTreeTest = (node:syntaxTreeNode) => {
    {
        label: node.label,
        children: node.children->Js_array2.map(c => {
            switch c {
                | Subtree(childNode) => Subtree(syntaxTreeToSyntaxTreeTest(childNode))
                | Symbol({sym}) => Symbol(sym)
            }
        })
    }
}


let testSyntaxTree = (~mmFile, ~exprStr, ~expectedSyntaxTree:syntaxTreeNodeTest) => {
    //given
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let expr = ctx->makeExprExn(exprStr->Js_string2.split(" "))
    let frms = prepareFrmSubsData(ctx)->Belt_MapString.toArray->Js_array2.map(((_,v)) => v)
    let parenCnt = parenCntMake(ctx->makeExprExn(["(", ")", "{", "}", "[", "]"]))
    let hyps = ctx->getAllHyps->Belt_MapString.toArray->Js_array2.map(((_,v)) => v)
    let tbl = []
    let targetIdx = findProof(~frms, ~parenCnt, ~expr, ~tbl, ~hyps, ~isDisjInCtx=ctx->isDisj)
    //proofTablePrint(ctx, proofTbl, "dddddddddddddd")

    //when
    let actualSyntaxTree = buildSyntaxTree(ctx, tbl, targetIdx)

    //then
    //Js.Console.log2("actualSyntaxTree", actualSyntaxTree->syntaxTreeToSyntaxTreeTest->Expln_utils_common.stringify)
    assertEqMsg(actualSyntaxTree->syntaxTreeToSyntaxTreeTest, expectedSyntaxTree, `testSyntaxTree for: ${exprStr}`)
}

describe("buildSyntaxTree", _ => {
    it("builds correct syntax trees for WWFs", _ => {
        let demo0 = "./src/metamath/test/resources/demo0.mm"
        let setReduced = "./src/metamath/test/resources/set-reduced.mm"

        testSyntaxTree(~mmFile=demo0, ~exprStr="wff t = t", 
            ~expectedSyntaxTree = {
                label: "weq",
                children: [
                    Subtree({
                        label: "tt",
                        children: [
                            Symbol("t")
                        ]
                    }),
                    Symbol("="),
                    Subtree({
                        label: "tt",
                        children: [ 
                            Symbol("t") 
                        ]
                    })
                ]
            }
        )

        testSyntaxTree(~mmFile=setReduced, ~exprStr="wff ( ( ph <-> ps ) <-> -. ( ( ph -> ps ) -> -. ( ps -> ph ) ) )", 
            ~expectedSyntaxTree = {
                label: "wb",
                children: [
                    Symbol("("),
                    Subtree({
                        label: "wb",
                        children: [
                            Symbol("("),
                            Subtree({
                                label: "wph",
                                children: [
                                    Symbol("ph")
                                ]
                            }),
                            Symbol("<->"),
                            Subtree({
                                label: "wps",
                                children: [
                                    Symbol("ps")
                                ]
                            }),
                            Symbol(")")
                        ]
                    }),
                    Symbol("<->"),
                    Subtree({
                        label: "wn",
                        children: [
                            Symbol("-."),
                            Subtree({
                                label: "wi",
                                children: [
                                    Symbol("("),
                                    Subtree({
                                        label: "wi",
                                        children: [
                                            Symbol("("),
                                            Subtree({
                                                label: "wph",
                                                children: [
                                                    Symbol("ph")
                                                ]
                                            }),
                                            Symbol("->"),
                                            Subtree({
                                                label: "wps",
                                                children: [
                                                    Symbol("ps")
                                                ]
                                            }),
                                            Symbol(")")
                                        ]
                                    }),
                                    Symbol("->"),
                                    Subtree({
                                        label: "wn",
                                        children: [
                                            Symbol("-."),
                                            Subtree({
                                                label: "wi",
                                                children: [
                                                    Symbol("("),
                                                    Subtree({
                                                        label: "wps",
                                                        children: [
                                                            Symbol("ps")
                                                        ]
                                                    }),
                                                    Symbol("->"),
                                                    Subtree({
                                                        label: "wph",
                                                        children: [
                                                            Symbol("ph")
                                                        ]
                                                    }),
                                                    Symbol(")")
                                                ]
                                            })
                                        ]
                                    }),
                                    Symbol(")")
                                ]
                            })
                        ]
                    }),
                    Symbol(")")
                ]
            }
        )
    })
})