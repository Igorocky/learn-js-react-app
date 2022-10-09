open Expln_React_Mui

@new external createFileReader: unit => {..} = "FileReader"

@react.component
let make = () => {
    <TextFileReader onChange={s=>
        Js.Console.log2("s", s)
    } />
}