require "xi.mini.grammar"
import Interpreter from require "xi.mini.interpreter"
import Visitor from require "xi.mini.visitor"

parse = (code) ->
  raw_ast = Parse code
  return Visitor\visit raw_ast

mini = (source) ->
  ast = parse source
  return Interpreter\eval ast

return { parse: parse, mini: mini }
