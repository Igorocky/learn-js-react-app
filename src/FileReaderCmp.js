var React = require("react");

export const FileReaderCmp = () => {
    //https://stackoverflow.com/questions/14446447/how-to-read-a-local-text-file-in-the-browser
    function pickAndReadFile(callback) {
        var el = document.createElement('input');
        el.setAttribute('type', 'file');
        el.style.display = 'none';
        document.body.appendChild(el);
        el.onchange = function (){
            const reader = new FileReader();
            reader.onload = function () {
                callback(reader.result);
                document.body.removeChild(el);
            };
            reader.readAsBinaryString(el.files[0]);
        }
        el.click();
    }
    return React.createElement('div', {onClick: () => pickAndReadFile(f=>console.log(f, 'f'))}, "FileReader---")
}
