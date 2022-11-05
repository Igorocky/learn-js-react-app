open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

type modalId = int

type modal = {
    id: modalId,
    render: unit => reElem
}

type openModalRef = React.ref<Js.Nullable.t<(unit=>reElem) => promise<modalId>>>

type state = {
    nextId: modalId,
    modals: array<modal>,
}

let createInitialState = () => {
    {
        nextId: 0,
        modals: [],
    }
}

let openModalPriv = (st, render) => {
    let id = st.nextId
    (
        {
            nextId: id+1,
            modals: st.modals->Js_array2.concat([{
                id,
                render
            }])
        },
        id
    )
}

let openModal:(openModalRef, unit => reElem) => promise<modalId> = (openModalRef, render) => {
    switch openModalRef.current->Js.Nullable.toOption {
        | None => Js.Exn.raiseError(`openModalRef.current is null`)
        | Some(openModal) => openModal(render)
    }
}

let updateModal = (st,id,newRender) => {
    {
        ...st,
        modals: st.modals->Js_array2.map(m => if m.id == id {{...m, render:newRender}} else {m})
    }
}

let closeModal = (st,id) => {
    {
        ...st,
        modals: st.modals->Js_array2.filter(m => m.id != id)
    }
}


@react.component
let make = (~openModalRef:openModalRef /*, ~updateModalRef, ~closeModalRef*/) => {
    let (state, setState) = React.useState(_ => createInitialState())

    openModalRef.current = React.useMemo0(() => {
        Js.Nullable.return(render => promise(rlv => {
            setState(prev => {
                let (st, id) = prev->openModalPriv(render)
                rlv(id)
                st
            })
        }))
    })

    let numOfModals = state.modals->Js.Array2.length
    if ( numOfModals == 0) {
        React.null
    } else {
        <Dialog opn=true>
            {state.modals[numOfModals-1].render()}
        </Dialog>
    }
}