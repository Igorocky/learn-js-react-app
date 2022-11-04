open MM_context
open MM_parser
open MM_proof_table

type rec syntaxTreeNode = {
    id: string,
    parent:option<syntaxTreeNode>,
    label:string,
    children:array<childNode>,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol(string)

let extractVarToRecIdxMapping = (args:array<int>, frame):array<int> => {
    let varToRecIdxMapping = Expln_utils_common.createArray(frame.numOfVars)
    let locks = Belt_Array.make(frame.numOfVars, false)
    if (args->Js_array2.length != frame.numOfArgs) {
        raise(MmException({msg:`extractVarToRecIdxMapping: args.Js_array2.length != frame.numOfArgs`}))
    }
    frame.hyps->Js_array2.forEachi((hyp,i) => {
        if (hyp.typ == F) {
            let v = hyp.expr[1]
            if (locks[v]) {
                raise(MmException({msg:`extractVarToRecIdxMapping: locks[v]`}))
            } else {
                locks[v] = true
                varToRecIdxMapping[v] = i
            }
        }
    })
    if (locks->Js_array2.some(lock => !lock)) {
        raise(MmException({msg:`extractVarToRecIdxMapping: locks->Js_array2.some(lock => !lock)`}))
    } else {
        varToRecIdxMapping
    }
}

let rec buildSyntaxTreeInner = (idSeq, ctx, tbl, parent, r):syntaxTreeNode => {
    switch r.proof {
        | None => raise(MmException({msg: `Unexpected condition: a proof table record to be used for syntax tree generation doesn't have a proof.`}))
        | Some(Hypothesis({label})) => {
            {
                id: idSeq(),
                parent,
                label,
                children: {
                    let maxI = r.expr->Js_array2.length - 1
                    let children = Expln_utils_common.createArray(maxI)
                    for i in 1 to maxI {
                        children[i-1] = Symbol(ctx->ctxIntToStrExn(r.expr[i]))
                    }
                    children
                }
            }
        }
        | Some(Assertion({args, label})) => {
            switch ctx->getFrame(label) {
                | None => raise(MmException({msg: `Cannot find a frame by label '${label}'`}))
                | Some(frame) => {
                    let varToRecIdxMapping = extractVarToRecIdxMapping(args, frame)
                    let this = {
                        id: idSeq(),
                        parent,
                        label,
                        children: Expln_utils_common.createArray(r.expr->Js_array2.length - 1)
                    }
                    r.expr->Js_array2.forEachi((s,i) => {
                        if (i > 0) {
                            this.children[i-1] = 
                                if (s < 0) {
                                    Symbol(ctx->ctxIntToStrExn(s))
                                } else {
                                    Subtree(buildSyntaxTreeInner(idSeq, ctx, tbl, Some(this), tbl[varToRecIdxMapping[s]]))
                                }
                        }
                    })
                    this
                }
            }
        }
    }
}

let buildSyntaxTree = (ctx, tbl, targetIdx):syntaxTreeNode => {
    let nextId = ref(0)
    let idSeq = () => {
        nextId.contents = nextId.contents + 1
        Belt_Int.toString(nextId.contents-1)
    }
    buildSyntaxTreeInner(idSeq, ctx, tbl, None, tbl[targetIdx])
}