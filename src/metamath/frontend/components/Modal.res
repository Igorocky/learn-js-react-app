open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise

type modalId = string

type modalMethods = {
    openModal: (unit=>reElem) => promise<modalId>,
    updateModal: (modalId, (unit => reElem)) => unit,
    closeModal: modalId => unit,
}

type modalRef = React.ref<Js.Nullable.t<modalMethods>>
let useModalRef = () => {
    React.useRef(Js.Nullable.null)
}

type openModalRef = React.ref<Js.Nullable.t<(unit=>reElem) => promise<modalId>>>
type updateModalRef = React.ref< Js.Nullable.t<(modalId, (unit => reElem)) => unit> >
type closeModalRef = React.ref< Js.Nullable.t<modalId => unit> >

let modalRefToModalMethods: modalRef => modalMethods = modalRef => {
    switch modalRef.current->Js.Nullable.toOption {
        | None => Js.Exn.raiseError(`modalRef.current is null`)
        | Some(modalMethods) => modalMethods
    }
}

let openModal:(modalRef, unit => reElem) => promise<modalId> = (modalRef, render) => modalRefToModalMethods(modalRef).openModal(render)
let updateModal:(modalRef, modalId, unit => reElem) => unit = (modalRef, modalId, render) => modalRefToModalMethods(modalRef).updateModal(modalId, render)
let closeModal:(modalRef, modalId) => unit = (modalRef, modalId) => modalRefToModalMethods(modalRef).closeModal(modalId)

type modal = {
    id: modalId,
    render: unit => reElem
}

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

let updateModalPriv = (st,id,newRender) => {
    {
        ...st,
        modals: st.modals->Js_array2.map(m => if m.id == id {{...m, render:newRender}} else {m})
    }
}

let closeModalPriv = (st,id) => {
    {
        ...st,
        modals: st.modals->Js_array2.filter(m => m.id != id)
    }
}

@react.component
let make = (~modalRef:modalRef) => {
    let (state, setState) = React.useState(createInitialState)

    modalRef.current = React.useMemo0(() => {
        Js.Nullable.return(
            {
                openModal: render => promise(rlv => {
                    setState(prev => {
                        let (st, id) = prev->openModalPriv(render)
                        rlv(id)
                        st
                    })
                }),
                updateModal: (modalId, render) => {
                    setState(updateModalPriv(_, modalId, render))
                },
                closeModal: modalId => {
                    setState(closeModalPriv(_, modalId))
                }
            }
        )
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