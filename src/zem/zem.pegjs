// To generate zem.parser.js:
// pegjs -e Zem.Parser zem.pegjs zem.parser.js

{
  function sexp(op, left, right) {
    return '(' + [op, left, right].join(' ') + ')'
  }

  var _vars = {}
  function registerVar(name) {
    _vars[name] = true
    return name
  }

  function vars() {
    return Object.keys(_vars);
  }
}

program
  = expr:expr others:(([\n;]+ expr:expr?) { return expr; })* {
    var smt2 = [expr].concat(others).filter(function(line) {
      return line != null
    });
    return [smt2,vars()];
  }

expr
  = _ left:add_expr _ op:bool_op _ right:add_expr _ {
    return sexp('assert',sexp(op, left, right))
  }

add_expr
  = left:mult_expr _ op:("+"/"-") _ right:add_expr {
    return sexp(op, left, right)
  }
  / mult_expr

mult_expr
  = left:atom _ op:("*"/"/") _ right:mult_expr {
    return sexp(op, left, right)
  }
  / atom

bool_op
  = "=" 
  / ">=" 
  / ">" 
  / "<" 
  / "<="

atom
  = var
  / integer

var "variable name"
  = letter:[a-z] rest:[a-z0-9]* {
    var id = [letter,rest.join('')].join('').toLowerCase(); 
    return registerVar(id);
  }

integer "integer"
  = "-" value:number { return -1 * value; }
  / number

number "number"
  = '0' { return 0; }
  / positive_number

positive_number "positive number"
  = head:[1-9] tail:[0-9]* { return parseInt(head + tail.join('')); }

_ "optional whitespace"
  = [ \t]*