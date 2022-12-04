open Expln_test
open MM_asrt_apply

describe("iterateCombinations", _ => {
    it("iterates all possible combinations", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,_)=>true,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            }
        )

        //then
        assertEq(
            res, 
            [
                "-1 -1",
                "-1 0",
                "-1 1",
                "-1 2",
                "0 -1",
                "0 0",
                "0 1",
                "0 2",
                "1 -1",
                "1 0",
                "1 1",
                "1 2",
                "2 -1",
                "2 0",
                "2 1",
                "2 2",
            ]
        )
    })
    it("iterates all possible combinations until stoped", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,_)=>true,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                if (comb[0] == 1 && comb[1] == -1) {
                    Stop
                } else {
                    Continue
                }
            }
        )

        //then
        assertEq(
            res, 
            [
                "-1 -1",
                "-1 0",
                "-1 1",
                "-1 2",
                "0 -1",
                "0 0",
                "0 1",
                "0 2",
                "1 -1",
            ]
        )
    })

    it("iterates all applicable combinations only", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (s,h) => mod( (s+h)->Js.Math.abs_int, 2) == 1,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            }
        )

        //then
        assertEq(
            res, 
            [
                "-1 0",
                "-1 2",
                "1 0",
                "1 2",
            ]
        )
    })

    it("doesn't iterate at all if there is at least one unmatched hypothesis", _ => {
        //given
        let res = []

        //when
        iterateCombinations(
            ~numOfStmts=3,
            ~numOfHyps=2,
            ~stmtCanMatchHyp = (_,h) => h != 0,
            ~combinationConsumer = comb => {
                res->Js_array2.push(comb->Js_array2.joinWith(" "))->ignore
                Continue
            }
        )

        //then
        assertEq( res, [ ] )
    })
})