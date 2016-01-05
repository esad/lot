Elm.Native = Elm.Native || {};
Elm.Native.Solver = Elm.Native.Solver || {};

Elm.Native.Solver.make = function(localRuntime) {
  if ('values' in Elm.Native.Solver) { return Elm.Native.Solver.values; }

  var List = Elm.Native.List.make(localRuntime);
  var Task = Elm.Native.Task.make(localRuntime);
  var Result = Elm.Result.make(localRuntime);
  var Utils = Elm.Native.Utils.make(localRuntime);
  var Tuple2 = Utils['Tuple2'];

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
              callback(Task.succeed({fs:FS, module:Module}));
            } else {
              console.error("Error loading ", z3_url);
              callback(Task.fail(request.status));
            }
          }
        };
        request.open("GET", z3_url);
        request.send();
      });
    },
    solve: F3(function(z3em, program, varsList) {
      var vars = List.toArray(varsList);

      console.log("Solving ", program, "with", z3em, vars);

      var stdout = [];
      var stderr = [];
      var before = "(push)\n" + vars.map(function(name) { return "(declare-const " + name + " Int)"; }).join("\n") + "\n";
      var after = "(check-sat)\n(get-value ("+vars.join(" ")+"))\n(pop)";
      var program = before + program + after;
      console.log("SMT:", program);
      z3em.fs.createDataFile("/", "input.smt2", program, true, true);
      try {
        z3em.module.print = function(str) { stdout.push(str); }
        z3em.module.printErr = function(str) { stderr.push(str);}
        z3em.module.callMain(["-smt2", "/input.smt2"])
      } catch (exception) {
        console.error("z3-emscripten exception:", exception);
      } finally {
        z3em.fs.unlink("/input.smt2");
      }

      if (stdout[0] == "sat") {
        var results = [];
        // concat the rest of stdout
        var solution = stdout.slice(1).join(' ');
        var re = /\(([^\(]+?) (.*?)\)/g;
        var match;
        // ..and scan it for (var value) pairs
        while (match = re.exec(solution)) {
          var name = match[1];
          var value = parseInt(match[2]);
          results.push(Tuple2(name, value));
        }
        return Result.Ok(List.fromArray(results));
      } else if (stdout[0] == "unsat") {
        console.log("No solution unsat")
        return Result.Err("unsat");
      } else {
        console.log("Error", stdout, stderr);
        return Result.Err("Internal solver error");
      }
    })
  } // return values
} // make