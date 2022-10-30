@new external createFileReader: unit => {..} = "FileReader"

@react.component
let make = (~onChange:(string,string)=>unit) => {
    <input
        type_="file" 
        onChange={evt=>{
            let fr = createFileReader()
            let file = ReactEvent.Synthetic.nativeEvent(evt)["target"]["files"][0]
            let fileName = file["name"]
            fr["onload"] = () => {
                let fileText = fr["result"]
                onChange(fileName, fileText)
                Js.Console.log2("fileName", fileName)
            }
            fr["readAsBinaryString"](. file )
        }}  
    /> 
}