var exports;

WebAssembly.instantiateStreaming(
    fetch("../all.wasm"),{
        console: {
            logNum : console.log
        }
    }
).then(results => 
    {
        exports = results.instance.exports
    }
)

function run_wasm(){
    var result=exports._start();
    document.querySelector('#res').textContent = `Result = ${result}`;
}