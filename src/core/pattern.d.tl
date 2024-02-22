local Cycle: number | Fraction

local record Fraction
  denominator: number
  numerator: number
end

local record Span
  _begin: Cycle
  _end: Cycle
end

local record State
  span: Span
  control: {}
end

local type Event = record<X>
  whole: nil | Span
  part: Span
  value: X
  context: {}
  stateful: boolean
end

local type Pattern = record<X>
  query: function(State): [Event<X>]
end

local value: number | string | { string : string } | Pattern --?

local silence: function(): Pattern<{}> --?

local pure: function(value): Pattern<value>

local mini: function(string): Pattern<value> --? string

local reify: function(any): Pattern<value> --?? not any??

-- local _patternify:

local tiemcat: function({{number, value}}): Pattern<value>

local fastcat: function(...: value): Pattern<value>

