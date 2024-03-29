global record Fraction
  denominator: number
  numerator: number
end

global record Span
  _begin: number | Fraction
  _end: number | Fraction
end

global record State
  span: Span
  control: {string: any}
end

-- global Value: number | string | { string : string } | Pattern<Value>

global type Event = record<X>
  whole: nil | Span
  part: Span
  value: X
  context: {string: any} -- TODO: fix for a union?
  stateful: boolean
end

global type Pattern = record<X>
  query: function(State): { Event<X> }
end

global silence: function(): Pattern<{nil}>

global pure: function<X>(x: X): Pattern<X>

global mini: function(string): Pattern<any> --? fix

global reify: function(any): Pattern<string> -- | Pattern<Pattern> --?? not any??

global timecat: function({{number, any}}): Pattern<any>

global fastcat: function(...: any): Pattern<any>

global sine2 = Pattern<number>

global sine = Pattern<number>

global cosine2 = Pattern<number>

global cosine = Pattern<number>

global square = Pattern<number>

global square2 = Pattern<number>

global isaw = Pattern<number>

global isaw2 = Pattern<number> 

global saw = Pattern<number>

global saw2 = Pattern<number>

global tri = Pattern<number> 

global tri2 = Pattern<number> 

global time = Pattern<number>

-- @section randoms
global rand = Pattern<number>

global fast = function<T>(time: Pattern<Time>, pat: Pattern<T>): Pattern<T>

