const wasm = require("./main.rs");
wasm.initialize({ noExitRuntime: true }).then(module => {
  window.prettify = module.cwrap("wasm_prettify", "string", ["string"]);
  window.init();
});
