Elm.Native = Elm.Native || {};
Elm.Native.Solver = Elm.Native.Solver || {};

Elm.Native.Solver.make = function(localRuntime) {
  if ('values' in Elm.Native.Solver) { return Elm.Native.Solver.values; }

  var Z3Em = function(fs, module) {
    this.fs = fs;
    this.module = module;
  };

  var Task = Elm.Native.Task.make(localRuntime);

  return Elm.Native.Solver.values = {
    load: function(z3_url) {
      return Task.asyncFunction(function(callback) {
        var request = new XMLHttpRequest();
        request.onreadystatechange = function () {
          var DONE = request.DONE || 4;
          if (request.readyState === DONE) {
            if (request.status == 200) {
              var Module = {
                noInitialRun : true,
                noExitRuntime : true,
                TOTAL_MEMORY : 128 * 1024 * 1024,
                memoryInitializerPrefixURL: (function() {
                  // Per default emscripten-generated js will try to load static memory map
                  // from /z3.js.mem - let's pass it the base path from which z3.js was loaded instead:
                  var uriParser = document.createElement('a');
                  uriParser.href = z3_url;
                  return uriParser.pathname.split('/').slice(0,-1).join('/')+'/';
                })()
              };
              console.log("Evaluating asmjs code...");
              eval(request.responseText);
              callback(Task.succeed(new Z3Em(FS, Module)));
            } else {
              console.error("Error loading ", z3_url);
              callback(Task.fail(request.status));
            }
          }
        };
        request.open("GET", z3_url);
        request.send();
      });
    }
  }
}