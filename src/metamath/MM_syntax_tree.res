open MM_context
open MM_parser
open MM_proof_table

type rec syntaxTreeNode = {
    parent:option<syntaxTreeNode>,
    label:string,
    children:array<childNode>,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol(int)

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

let rec buildSyntaxTreeInner = (ctx, tbl, parent, r):syntaxTreeNode => {
    switch r.proof {
        | None => raise(MmException({msg: `Unexpected condition: a proof table record to be used for syntax tree generation doesn't have a proof.`}))
        | Some(Hypothesis({label})) => {
            {
                parent,
                label,
                children: r.expr->Js_array2.map(s => Symbol(s))
            }
        }
        | Some(Assertion({args, label})) => {
            switch ctx->getFrame(label) {
                | None => raise(MmException({msg: `Cannot find a frame by label '${label}'`}))
                | Some(frame) => {
                    let varToRecIdxMapping = extractVarToRecIdxMapping(args, frame)
                    let this = {
                        parent,
                        label,
                        children: []
                    }
                    r.expr->Js_array2.forEach(s => {
                        this.children->Js_array2.push(
                            if (s < 0) {
                                Symbol(s)
                            } else {
                                Subtree(buildSyntaxTreeInner(ctx, tbl, Some(this), tbl[varToRecIdxMapping[s]]))
                            }
                        )->ignore
                    })
                    this
                }
            }
        }
    }
}

let buildSyntaxTree = (ctx, tbl, targetIdx):syntaxTreeNode => {
    buildSyntaxTreeInner(ctx, tbl, None, tbl[targetIdx])
}