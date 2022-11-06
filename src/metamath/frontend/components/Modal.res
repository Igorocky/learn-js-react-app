open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

type modalId = string

type modal = {
    id: modalId,
    render: unit => reElem
}

type openModalRef = React.ref<Js.Nullable.t<(unit=>reElem) => promise<modalId>>>
type updateModalRef = React.ref< Js.Nullable.t<(modalId, (unit => reElem)) => unit> >
type closeModalRef = React.ref< Js.Nullable.t<modalId => unit> >

type state = {
    nextId: int,
    modals: array<modal>,
}

let createInitialState = () => {
    {
        nextId: 0,
        modals: [],
    }
}

let openModalPriv = (st, render) => {
    let id = st.nextId->Belt_Int.toString
    (
        {
            nextId: st.nextId+1,
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

let updateModalPriv = (st,id,newRender) => {
    {
        ...st,
        modals: st.modals->Js_array2.map(m => if m.id == id {{...m, render:newRender}} else {m})
    }
}

let updateModal: (updateModalRef, modalId, unit => reElem) => unit = (updateModalRef, modalId, render) => {
    switch updateModalRef.current->Js.Nullable.toOption {
        | None => Js.Exn.raiseError(`updateModalRef.current is null`)
        | Some(updateModal) => updateModal(modalId, render)
    }
}

let closeModalPriv = (st,id) => {
    {
        ...st,
        modals: st.modals->Js_array2.filter(m => m.id != id)
    }
}

let closeModal: (closeModalRef, modalId) => unit = (closeModalRef, modalId) => {
    switch closeModalRef.current->Js.Nullable.toOption {
        | None => Js.Exn.raiseError(`closeModalRef.current is null`)
        | Some(closeModal) => closeModal(modalId)
    }
}

@react.component
let make = (~openModalRef:openModalRef, ~updateModalRef:updateModalRef, ~closeModalRef:closeModalRef) => {
    let (state, setState) = React.useState(createInitialState)

    openModalRef.current = React.useMemo0(() => {
        Js.Nullable.return(render => promise(rlv => {
            setState(prev => {
                let (st, id) = prev->openModalPriv(render)
                rlv(id)
                st
            })
        }))
    })

    updateModalRef.current = React.useMemo0(() => {
        Js.Nullable.return((modalId, render) => {
            setState(updateModalPriv(_, modalId, render))
        })
    })

    closeModalRef.current = React.useMemo0(() => {
        Js.Nullable.return(modalId => {
            setState(closeModalPriv(_, modalId))
        })
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