{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'

    import * as n from './visitor/CST.js';
}}

gramatica
  = _ code:globalCode? prods:regla+ _ {
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    prods[0].start = true;
    return new n.Grammar(prods, code);
  }

globalCode
  = "{" before:$(. !"contains")* [ \t\n\r]* "contains" [ \t\n\r]* after:$[^}]* "}" {
    return after ? {before, after} : {before}
  }

regla
  = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Regla(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:parsingExpression rest:(_ @parsingExpression !(_ literales? _ "=") )* action:(_ @predicate)? {
    const exprs = [expr, ...rest];
    const labeledExprs = exprs
        .filter((expr) => expr instanceof n.Pluck)
        .filter((expr) => expr.labeledExpr.label);
    if (labeledExprs.length > 0) {
        action.params = labeledExprs.reduce((args, labeled) => {
            const expr = labeled.labeledExpr.annotatedExpr.expr;
            args[labeled.labeledExpr.label] =
                expr instanceof n.Identificador ? expr.id : '';
            return args;
        }, {});
    }
    return new n.Union(exprs, action);
  }

parsingExpression
  = pluck
  / '!' assertion:(match/predicate) {
    return new n.NegAssertion(assertion);
  }
  / '&' assertion:(match/predicate) {
    return new n.Assertion(assertion);
  }
  / "!." {
    return new n.Fin();
  }

pluck
  = pluck:"@"? _ expr:label {
    return new n.Pluck(expr, pluck ? true : false);
  }

label
  = label:(@identificador _ ":")? _ expr:annotated {
    return new n.Label(expr, label);
  }

annotated
  = text:"$"? _ expr:match _ qty:([?+*]/conteo)? {
    return new n.Annotated(expr, qty, text ? true : false);
  }

match
  = id:identificador {
    usos.push(id)
    return new n.Identificador(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase ? true : false,false);
  }
  / "(" _ nodo:opciones _ ")" {
    return new n.Agrupacion(nodo.exprs);
  }
  / chars:clase isCase:"i"? {
    return new n.Clase(chars, isCase ? true : false);
  }
  / "." {
    return new n.Punto();
  }

conteo
  = "|" _ valor1:(numero / id:identificador) _ "|" { // expression |count|
    return {min: valor1, caso: "caso1"};
  }
  / "|" _ valor1:(numero / id:identificador)? _ ".." _ valor2:(numero / id2:identificador)? _ "|" { // expression |min..max|
    return {min: valor1, max: valor2, caso: "caso2"};
  }
  / "|" _ valor1:(numero / id:identificador)? _ "," _ intermedio:opciones _ "|" { // expression |count, delimiter|
    return {min: valor1, intermedio: intermedio, caso: "caso3"};
  }
  / "|" _ valor1:(numero / id:identificador)? _ ".." _ valor2:(numero / id2:identificador)? _ "," _ intermedio:opciones _ "|"  // expression |min..max, delimiter|
  {
    return {min: valor1, max: valor2, intermedio: intermedio, caso: "caso4"};
  }

predicate
  = "{" [ \t\n\r]* returnType:predicateReturnType code:$[^}]* "}" {
    return new n.Predicate(returnType, code, {})
  }

predicateReturnType
  = t:$(. !"::")+ [ \t\n\r]* "::" [ \t\n\r]* "res" {
    return t.trim();
  }

clase
  = "[" @contenidoClase+ "]"

contenidoClase
  = bottom:$caracter "-" top:$caracter {
    return new n.Rango(bottom, top);
  }
  / $caracter

caracter
  = [^\[\]\\]
  / "\\" .

literales
  = '"' @stringDobleComilla* '"'
  / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    
numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }

_ = (Comentarios /[ \t\n\r])*

Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"