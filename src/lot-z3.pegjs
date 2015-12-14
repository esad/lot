{
  function sexp(op, left, right) {
    return '(' + [op, left, right].join(' ') + ')'
  }

  var _constants = {}
  function registerConstant(id) {
    _constants[id] = true
    return id
  }

  function constants() {
    return Object.keys(_constants).map(function(id) {
      return sexp("declare-const", id, "Int");
    })
  }
}

program
  = expr:expr others:(([\n;]+ expr:expr?) { return expr; })* {
    return constants().concat([expr]).concat(others).filter(function(el) {
      return el != null
    }).join("\n")
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
  = cell
  / integer

cell "cell identifier"
  = col:[A-Za-z] row:positive_number {
    var id = [col,row].join('').toLowerCase(); 
    return registerConstant(id)
  }

integer "integer"
  = "-" value:number { return -1 * value; }
  / number

number "number"
  = '0' { return 0; }
  / positive_number

positive_number "positive number"
  = head:[1-9] tail:[0-9]* { return parseInt(head + tail.join('')); }

letter "letter"
  = [A-Za-z]

_ "optional whitespace"
  = [ \t]*