local map, filter, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type, bjork, parseChord, getScale, Fraction, tofrac, tofloat, genericParams, aliasParams, Event, Span, State, parse, op, string_lambda, fun, applyOptions, resolveReplications, patternifyAST, mini, sin, min, max, pi, floor, tinsert, C, create, notemt, Pattern, silence, pure, reify, _patternify, _patternify_p_p, _patternify_p_p_p, stack, slowcatPrime, slowcat, fastcat, timecat, _cpm, cpm, _fast, fast, _slow, slow, _early, early, _late, late, _inside, inside, _outside, outside, _ply, ply, _fastgap, fastgap, _compress, compress, _focus, focusSpan, focus, _zoom, zoom, run, scan, waveform, steady, toBipolar, fromBipolar, sine2, sine, cosine2, cosine, square, square2, isaw, isaw2, saw, saw2, tri, tri2, time, rand, _irand, irand, _chooseWith, chooseWith, choose, chooseCycles, randcat, polyrhythm, _degradeByWith, _degradeBy, degradeBy, undegradeBy, _undegradeBy, degrade, undegrade, sometimesBy, sometimes, struct, _euclid, euclid, rev, palindrome, _iter, iter, _reviter, reviter, _segment, segment, _range, range, superimpose, layer, _off, off, _echoWith, echoWith, _when, when_, _firstOf, firstOf, every, _lastOf, lastOf, _jux, jux, _juxBy, juxBy, _striate, striate, _chop, chop, slice, splice, _loopAt, loopAt, fit, _legato, legato, _scale, scale, apply, sl
do
  local _obj_0 = require("xi.utils")
  map, filter, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type = _obj_0.map, _obj_0.filter, _obj_0.reduce, _obj_0.id, _obj_0.flatten, _obj_0.totable, _obj_0.dump, _obj_0.concat, _obj_0.rotate, _obj_0.union, _obj_0.timeToRand, _obj_0.curry, _obj_0.type
end
bjork = require("xi.euclid").bjork
parseChord = require("xi.chords").parseChord
getScale = require("xi.scales").getScale
do
  local _obj_0 = require("xi.fraction")
  Fraction, tofrac, tofloat = _obj_0.Fraction, _obj_0.tofrac, _obj_0.tofloat
end
do
  local _obj_0 = require("xi.control")
  genericParams, aliasParams = _obj_0.genericParams, _obj_0.aliasParams
end
do
  local _obj_0 = require("xi.types")
  Event, Span, State = _obj_0.Event, _obj_0.Span, _obj_0.State
end
parse = require("xi.mini.grammar").parse
op = require("fun").op
string_lambda = require("pl.utils").string_lambda
fun = require("fun")
applyOptions = function(parent, enter)
  return function(pat, i)
    local ast = parent.source[i]
    local ops = nil
    if ast.options then
      ops = ast.options.ops
    end
    if ops then
      for _index_0 = 1, #ops do
        local op = ops[_index_0]
        local _exp_0 = op.type
        if "stretch" == _exp_0 then
          local type_, amount = op.arguments.type, op.arguments.amount
          local _exp_1 = type_
          if "fast" == _exp_1 then
            pat = fast(enter(amount), pat)
          elseif "slow" == _exp_1 then
            pat = slow(enter(amount), pat)
          else
            print("mini: stretch: type must be one of fast of slow")
          end
        elseif "euclid" == _exp_0 then
          local steps, pulse, rotation = op.arguments.steps, op.arguments.pulse, op.arguments.rotation
          pat = euclid(enter(pulse), enter(steps), enter(rotation), pat)
        end
      end
    end
    return pat
  end
end
resolveReplications = function(ast)
  local repChild
  repChild = function(child)
    if child.options == nil then
      return {
        child
      }
    end
    local reps = child.options.reps
    child.options.reps = nil
    local _accum_0 = { }
    local _len_0 = 1
    for i = 1, reps do
      _accum_0[_len_0] = child
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end
  local unflat
  do
    local _accum_0 = { }
    local _len_0 = 1
    local _list_0 = ast.source
    for _index_0 = 1, #_list_0 do
      local child = _list_0[_index_0]
      _accum_0[_len_0] = repChild(child)
      _len_0 = _len_0 + 1
    end
    unflat = _accum_0
  end
  local res = { }
  for _index_0 = 1, #unflat do
    local element = unflat[_index_0]
    for _index_1 = 1, #element do
      local elem = element[_index_1]
      tinsert(res, elem)
    end
  end
  ast.source = res
  return ast
end
patternifyAST = function(ast)
  local enter
  enter = function(node)
    return patternifyAST(node)
  end
  local _exp_0 = ast.type
  if "pattern" == _exp_0 then
    ast = resolveReplications(ast)
    local children = ast.source
    children = map(enter, children)
    do
      local _accum_0 = { }
      local _len_0 = 1
      for index, child in pairs(children) do
        _accum_0[_len_0] = applyOptions(ast, enter)(child, index)
        _len_0 = _len_0 + 1
      end
      children = _accum_0
    end
    local alignment = ast.arguments.alignment
    local _exp_1 = alignment
    if "stack" == _exp_1 then
      return stack(children)
    end
    local addWeight
    addWeight = function(a, b)
      return a + b.options.weight or 1
    end
    local weightSum = reduce(addWeight, 0, ast.source)
    if weightSum > #children then
      local atoms = ast.source
      local pat = timecat((function()
        local _accum_0 = { }
        local _len_0 = 1
        for i, v in pairs(atoms) do
          _accum_0[_len_0] = {
            v.options.weight or 1,
            children[i]
          }
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)())
      return pat
    end
    return fastcat(children)
  elseif "element" == _exp_0 then
    return enter(ast.source)
  elseif "atom" == _exp_0 then
    if ast.source == "~" then
      return silence()
    end
    local value = ast.source
    if (tonumber(value)) then
      value = tonumber(value)
    end
    return pure(value)
  end
end
mini = function(code)
  local ast = parse(code)
  return patternifyAST(ast)
end
sin = math.sin
min = math.min
max = math.max
pi = math.pi
floor = math.floor
tinsert = table.insert
C = { }
create = function(name)
  local withVal
  withVal = function(v)
    return {
      [name] = v
    }
  end
  local func
  func = function(args)
    return reify(args):fmap(withVal)
  end
  C[name] = func
end
local _list_0 = genericParams
for _index_0 = 1, #_list_0 do
  local name = _list_0[_index_0]
  local param = name[2]
  create(param)
  if aliasParams[param] ~= nil then
    local alias = aliasParams[param]
    C[alias] = C[param]
  end
end
notemt = {
  __add = function(self, other)
    return {
      note = self.note + other.note
    }
  end
}
C.note = function(args)
  args = reify(args)
  local chordToStack
  chordToStack = function(thing)
    return stack(parseChord(thing))
  end
  local withVal
  withVal = function(v)
    return setmetatable({
      note = v
    }, notemt)
  end
  return reify(args):fmap(chordToStack):outerJoin():fmap(withVal)
end
do
  local _class_0
  local _base_0 = {
    type = function()
      return "pattern"
    end,
    querySpan = function(self, b, e)
      local span = Span(b, e)
      local state = State(span)
      return self:query(state)
    end,
    __call = function(self, b, e)
      return self:querySpan(b, e)
    end,
    firstCycle = function(self)
      return self(0, 1)
    end,
    __tostring = function(self)
      local func
      func = function(event)
        return event:show()
      end
      return dump(map(func, self:firstCycle()))
    end,
    show = function(self)
      return self:__tostring()
    end,
    bindWhole = function(self, choose_whole, func)
      local query
      query = function(_, state)
        local withWhole
        withWhole = function(a, b)
          local new_whole = choose_whole(a.whole, b.whole)
          return Event(new_whole, b.part, b.value)
        end
        local match
        match = function(a)
          local events = func(a.value)(a.part._begin, a.part._end)
          local f
          f = function(b)
            return withWhole(a, b)
          end
          return map(f, events)
        end
        local events = self:query(state)
        return flatten((map((function(a)
          return match(a)
        end), events)))
      end
      return Pattern(query)
    end,
    outerBind = function(self, func)
      return self:bindWhole((function(a)
        return a
      end), func)
    end,
    innerBind = function(self, func)
      return self:bindWhole((function(_, b)
        return b
      end), func)
    end,
    outerJoin = function(self)
      return self:outerBind(id)
    end,
    innerJoin = function(self)
      return self:innerBind(id)
    end,
    squeezeJoin = function(self)
      local query
      query = function(_, state)
        local events = self:discreteOnly():query(state)
        local flatEvent
        flatEvent = function(outerEvent)
          local innerPat = focusSpan(outerEvent:wholeOrPart(), outerEvent.value)
          local innerEvents = innerPat:query(State(outerEvent.part))
          local munge
          munge = function(outer, inner)
            local whole = nil
            if inner.whole and outer.whole then
              whole = inner.whole:sect(outer.whole)
              if not whole then
                return nil
              end
            end
            local part = inner.part:sect(outer.part)
            if not part then
              return nil
            end
            return Event(whole, part, inner.value)
          end
          local f
          f = function(innerEvent)
            return munge(outerEvent, innerEvent)
          end
          return map(f, innerEvents)
        end
        local result = flatten(map(flatEvent, events))
        return filter((function(x)
          return x
        end), result)
      end
      return Pattern(query)
    end,
    squeezeBind = function(self, func)
      return self:fmap(func):squeezeJoin()
    end,
    filterEvents = function(self, func)
      local query
      query = function(_, state)
        local events = self:query(state)
        return filter(func, events)
      end
      return Pattern(query)
    end,
    filterValues = function(self, condf)
      local query
      query = function(_, state)
        local events = self:query(state)
        local f
        f = function(event)
          return condf(event.value)
        end
        return filter(f, events)
      end
      return Pattern(query)
    end,
    removeNils = function(self)
      return self:filterValues(function(v)
        return v ~= nil
      end)
    end,
    splitQueries = function(self)
      local query
      query = function(_, state)
        local cycles = state.span:spanCycles()
        local f
        f = function(span)
          local sub_state = state:setSpan(span)
          return self:query(sub_state)
        end
        return flatten(map(f, cycles))
      end
      return Pattern(query)
    end,
    withValue = function(self, func)
      local query
      query = function(_, state)
        local events = self:query(state)
        local f
        f = function(event)
          return event:withValue(func)
        end
        return map(f, events)
      end
      return Pattern(query)
    end,
    fmap = function(self, func)
      return self:withValue(func)
    end,
    withQuerySpan = function(self, func)
      local query
      query = function(_, state)
        local new_state = state:withSpan(func)
        return self:query(new_state)
      end
      return Pattern(query)
    end,
    withQueryTime = function(self, func)
      return self:withQuerySpan(function(span)
        return span:withTime(func)
      end)
    end,
    withTime = function(self, qf, ef)
      local query = self:withQueryTime(qf)
      local pattern = query:withEventTime(ef)
      return pattern
    end,
    withEvents = function(self, func)
      return Pattern(function(_, state)
        return func(self:query(state))
      end)
    end,
    withEvent = function(self, func)
      return self:withEvents(function(events)
        return map(func, events)
      end)
    end,
    withEventSpan = function(self, func)
      local query
      query = function(_, state)
        local events = self:query(state)
        local f
        f = function(event)
          return event:withSpan(func)
        end
        return map(f, events)
      end
      return Pattern(query)
    end,
    withEventTime = function(self, func)
      local query
      query = function(_, state)
        local events = self:query(state)
        local time_func
        time_func = function(span)
          return span:withTime(func)
        end
        local event_func
        event_func = function(event)
          return event:withSpan(time_func)
        end
        return map(event_func, events)
      end
      return Pattern(query)
    end,
    onsetsOnly = function(self)
      return self:filterEvents(function(event)
        return event:hasOnset()
      end)
    end,
    discreteOnly = function(self)
      return self:filterEvents(function(event)
        return event.whole
      end)
    end,
    appLeft = function(self, pat_val)
      local query
      query = function(_, state)
        local events = { }
        local event_funcs = self:query(state)
        for _index_0 = 1, #event_funcs do
          local event_func = event_funcs[_index_0]
          local whole = event_func:wholeOrPart()
          local event_vals = pat_val(whole._begin, whole._end)
          for _index_1 = 1, #event_vals do
            local event_val = event_vals[_index_1]
            local new_whole = event_func.whole
            local new_part = event_func.part:sect(event_val.part)
            local new_context = event_val:combineContext(event_func)
            if new_part ~= nil then
              local new_value = event_func.value(event_val.value)
              tinsert(events, Event(new_whole, new_part, new_value, new_context))
            end
          end
        end
        return events
      end
      return Pattern(query)
    end,
    appRight = function(self, pat_val)
      local query
      query = function(_, state)
        local events = { }
        local event_vals = pat_val:query(state)
        for _index_0 = 1, #event_vals do
          local event_val = event_vals[_index_0]
          local whole = event_val:wholeOrPart()
          local event_funcs = self(whole._begin, whole._end)
          event_val:wholeOrPart()
          for _index_1 = 1, #event_funcs do
            local event_func = event_funcs[_index_1]
            local new_whole = event_val.whole
            local new_part = event_func.part:sect(event_val.part)
            local new_context = event_val:combineContext(event_func)
            if new_part ~= nil then
              local new_value = event_func.value(event_val.value)
              tinsert(events, Event(new_whole, new_part, new_value, new_context))
            end
          end
          return events
        end
      end
      return Pattern(query)
    end,
    combineRight = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return union(y, x)
        end
      end):appLeft(other)
    end,
    combineLeft = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return union(x, y)
        end
      end):appLeft(other)
    end,
    __eq = function(self, other)
      return self:show() == other:show()
    end,
    __mul = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return y * x
        end
      end):appLeft(reify(other))
    end,
    __div = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return y / x
        end
      end):appLeft(reify(other))
    end,
    __add = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return y + x
        end
      end):appLeft(reify(other))
    end,
    __sub = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return y - x
        end
      end):appLeft(reify(other))
    end,
    __mod = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return y % x
        end
      end):appLeft(reify(other))
    end,
    __pow = function(self, other)
      return self:fmap(function(x)
        return function(y)
          return y ^ x
        end
      end):appLeft(reify(other))
    end,
    __concat = function(self, other)
      return self:combineLeft(other)
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, query)
      if query == nil then
        query = function()
          return { }
        end
      end
      self.query = query
    end,
    __base = _base_0,
    __name = "Pattern"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Pattern = _class_0
end
silence = function()
  return Pattern()
end
pure = function(value)
  local query
  query = function(self, state)
    local cycles = state.span:spanCycles()
    local f
    f = function(span)
      local whole = span:wholeCycle(span._begin)
      return Event(whole, span, value)
    end
    return map(f, cycles)
  end
  return Pattern(query)
end
reify = function(thing)
  local _exp_0 = type(thing)
  if "string" == _exp_0 then
    if string_lambda(thing) then
      return string_lambda(thing)
    else
      return mini(thing)
    end
  elseif "pattern" == _exp_0 then
    return thing
  else
    return pure(thing)
  end
end
_patternify = function(func)
  local patterned
  patterned = function(apat, pat)
    if pat == nil then
      return curry(func, 2)(apat)
    end
    apat = reify(apat)
    local mapFn
    mapFn = function(a)
      return func(a, pat)
    end
    return apat:fmap(mapFn):innerJoin()
  end
  return patterned
end
_patternify_p_p = function(func)
  local patterned
  patterned = function(apat, bpat, pat)
    if pat == nil then
      return curry(func, 3)(apat)(bpat)
    end
    apat, bpat, pat = reify(apat), reify(bpat), reify(pat)
    local mapFn
    mapFn = function(a, b)
      return func(a, b, pat)
    end
    mapFn = curry(mapFn, 2)
    local patOfPats = apat:fmap(mapFn):appLeft(bpat)
    return patOfPats:innerJoin()
  end
  return patterned
end
_patternify_p_p_p = function(func)
  local patterned
  patterned = function(apat, bpat, cpat, pat)
    if pat == nil then
      return curry(func, 4)(apat)(bpat)(cpat)
    end
    apat, bpat, cpat, pat = reify(apat), reify(bpat), reify(cpat), reify(pat)
    local mapFn
    mapFn = function(a, b, c)
      return func(a, b, c, pat)
    end
    mapFn = curry(mapFn, 3)
    local patOfPats = apat:fmap(mapFn):appLeft(bpat):appLeft(cpat)
    return patOfPats:innerJoin()
  end
  return patterned
end
stack = function(...)
  local pats = map(reify, totable(...))
  local query
  query = function(self, state)
    local span = state.span
    local f
    f = function(pat)
      return pat(span._begin, span._end)
    end
    local events = map(f, pats)
    return flatten(events)
  end
  return Pattern(query)
end
slowcatPrime = function(...)
  local pats = map(reify, totable(...))
  local query
  query = function(self, state)
    local len = #pats
    local index = state.span._begin:sam():asFloat() % len + 1
    local pat = pats[index]
    return pat:query(state)
  end
  return Pattern(query):splitQueries()
end
slowcat = function(...)
  local pats = map(reify, totable(...))
  local query
  query = function(self, state)
    local span = state.span
    local len = #pats
    local index = state.span._begin:sam():asFloat() % len + 1
    local pat = pats[index]
    if not pat then
      return { }
    end
    local offset = span._begin:floor() - (span._begin / len):floor()
    return pat:withEventTime(function(t)
      return t + offset
    end):query(state:setSpan(span:withTime(function(t)
      return t - offset
    end)))
  end
  return Pattern(query):splitQueries()
end
fastcat = function(...)
  local pats = map(reify, totable(...))
  return _fast(#pats, slowcat(...))
end
timecat = function(tuples)
  local pats = { }
  local times = map((function(x)
    return x[1]
  end), tuples)
  local total = reduce(op.add, Fraction(0), times)
  local accum = Fraction(0)
  for _index_0 = 1, #tuples do
    local tup = tuples[_index_0]
    local pat
    time, pat = tup[1], tup[2]
    local b = accum / total
    local e = (accum + time) / total
    local new_pat = _compress(b, e, pat)
    tinsert(pats, new_pat)
    accum = accum + time
  end
  return stack(pats)
end
_cpm = function(cpm, pat)
  return (reify(pat)):_fast(cpm / 60)
end
cpm = _patternify(_cpm)
_fast = function(factor, pat)
  return pat:withTime((function(t)
    return t * factor
  end), (function(t)
    return t / factor
  end))
end
fast = _patternify(_fast)
_slow = function(factor, pat)
  return _fast((1 / factor), pat)
end
slow = _patternify(_slow)
_early = function(offset, pat)
  return pat:withTime((function(t)
    return t + offset
  end), (function(t)
    return t - offset
  end))
end
early = _patternify(_early)
_late = function(offset, pat)
  return _early(-offset, pat)
end
late = _patternify(_late)
_inside = function(factor, f, pat)
  return _fast(factor, f(_slow(factor, pat)))
end
inside = _patternify_p_p(_inside)
_outside = function(factor, f, pat)
  return _inside(1 / factor, f, pat)
end
outside = _patternify_p_p(_outside)
_ply = function(factor, pat)
  pat = pure(_fast(factor, pat))
  return pat:squeezeJoin()
end
ply = _patternify(_ply)
_fastgap = function(factor, pat)
  factor = tofrac(factor)
  if factor <= Fraction(0) then
    return silence()
  end
  factor = factor:max(1)
  local mungeQuery
  mungeQuery = function(t)
    return t:sam() + ((t - t:sam()) * factor):min(1)
  end
  local eventSpanFunc
  eventSpanFunc = function(span)
    local b = span._begin:sam() + (span._begin - span._begin:sam()) / factor
    local e = span._begin:sam() + (span._end - span._begin:sam()) / factor
    return Span(b, e)
  end
  local query
  query = function(_, state)
    local span = state.span
    local new_span = Span(mungeQuery(span._begin), mungeQuery(span._end))
    if new_span._begin == new_span._begin:nextSam() then
      return { }
    end
    local new_state = State(new_span)
    local events = pat:query(new_state)
    local f
    f = function(event)
      return event:withSpan(eventSpanFunc)
    end
    return map(f, events)
  end
  return Pattern(query):splitQueries()
end
fastgap = _patternify(_fastgap)
_compress = function(b, e, pat)
  b, e = tofrac(b), tofrac(e)
  if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0) then
    return silence()
  end
  local fasted = _fastgap((Fraction(1) / (e - b)), pat)
  return _late(b, fasted)
end
compress = _patternify_p_p(_compress)
_focus = function(b, e, pat)
  b, e = tofrac(b), tofrac(e)
  local fasted = _fast((Fraction(1) / (e - b)), pat)
  return _late(Span:cyclePos(b), fasted)
end
focusSpan = function(span, pat)
  return _focus(span._begin, span._end, reify(pat))
end
focus = _patternify_p_p(_focus)
_zoom = function(s, e, pat)
  s, e = tofrac(s), tofrac(e)
  local dur = e - s
  local qf
  qf = function(span)
    return span:withCycle(function(t)
      return t * dur + s
    end)
  end
  local ef
  ef = function(span)
    return span:withCycle(function(t)
      return (t - s) / dur
    end)
  end
  return pat:withQuerySpan(qf):withEventSpan(ef):splitQueries()
end
zoom = _patternify_p_p(_zoom)
run = function(n)
  return fastcat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for i in range(0, n - 1) do
      _accum_0[_len_0] = i
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
scan = function(n)
  return slowcat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for i in range(n) do
      _accum_0[_len_0] = run(i)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
waveform = function(func)
  local query
  query = function(self, state)
    return {
      Event(nil, state.span, func(state.span:midpoint()))
    }
  end
  return Pattern(query)
end
steady = function(value)
  return Pattern(function(state)
    return {
      Event(nil, state.span, value)
    }
  end)
end
toBipolar = function(pat)
  return pat:fmap(function(x)
    return x * 2 - 1
  end)
end
fromBipolar = function(pat)
  return pat:fmap(function(x)
    return (x + 1) / 2
  end)
end
sine2 = waveform(function(t)
  return sin(t:asFloat() * pi * 2)
end)
sine = fromBipolar(sine2)
cosine2 = _late(1 / 4, sine2)
cosine = fromBipolar(cosine2)
square = waveform(function(t)
  return floor((t * 2) % 2)
end)
square2 = toBipolar(square)
isaw = waveform(function(t)
  return -(t % 1) + 1
end)
isaw2 = toBipolar(isaw)
saw = waveform(function(t)
  return t % 1
end)
saw2 = toBipolar(saw)
tri = fastcat(isaw, saw)
tri2 = fastcat(isaw2, saw2)
time = waveform(id)
rand = waveform(timeToRand)
_irand = function(i)
  return rand:fmap(function(x)
    return floor(x * i)
  end)
end
irand = function(ipat)
  return reify(ipat):fmap(_irand):innerJoin()
end
_chooseWith = function(pat, ...)
  local vals = map(reify, totable(...))
  if #vals == 0 then
    return silence()
  end
  return range(1, #vals + 1, pat):fmap(function(i)
    local key = min(max(floor(i), 0), #vals)
    return vals[key]
  end)
end
chooseWith = function(pat, ...)
  return _chooseWith(pat, ...):outerJoin()
end
choose = function(...)
  return chooseWith(rand, ...)
end
chooseCycles = function(...)
  return segment(1, choose(...))
end
randcat = function(...)
  return chooseCycles(...)
end
polyrhythm = function(...)
  return stack(...)
end
_degradeByWith = function(prand, by, pat)
  pat = reify(pat)
  if type(by) == "fraction" then
    by = by:asFloat()
  end
  local f
  f = function(v)
    return v > by
  end
  return pat:fmap(function(val)
    return function(_)
      return val
    end
  end):appLeft(prand:filterValues(f))
end
_degradeBy = function(by, pat)
  return _degradeByWith(rand, by, pat)
end
degradeBy = _patternify(_degradeBy)
undegradeBy = _patternify(_undegradeBy)
_undegradeBy = function(by, pat)
  return _degradeByWith(rand:fmap(function(r)
    return 1 - r
  end), by, pat)
end
degrade = function(pat)
  return _degradeBy(0.5, pat)
end
undegrade = function(pat)
  return _undegradeBy(0.5, pat)
end
sometimesBy = function(by, func, pat)
  return (reify(by):fmap(function(by)
    return stack(_degradeBy(by, pat), func(_undegradeBy(1 - by, pat)))
  end)):innerJoin()
end
sometimes = function(func, pat)
  return sometimesBy(0.5, func, pat)
end
struct = function(boolpat, pat)
  return (fastcat(boolpat)):fmap(function(b)
    return function(val)
      if b then
        return val
      else
        return nil
      end
    end
  end):appLeft(pat):removeNils()
end
_euclid = function(n, k, offset, pat)
  return struct(bjork(n, k, offset), reify(pat))
end
euclid = _patternify_p_p_p(_euclid)
rev = function(pat)
  local query
  query = function(_, state)
    local span = state.span
    local cycle = span._begin:sam()
    local nextCycle = span._begin:nextSam()
    local reflect
    reflect = function(to_reflect)
      local reflected = to_reflect:withTime(function(time)
        return cycle + (nextCycle - time)
      end)
      local tmp = reflected._begin
      reflected._begin = reflected._end
      reflected._end = tmp
      return reflected
    end
    local events = pat:query(state:setSpan(reflect(span)))
    return map((function(event)
      return event:withSpan(reflect)
    end), events)
  end
  return Pattern(query)
end
palindrome = function(pat)
  return slowcat(pat, rev(pat))
end
_iter = function(n, pat)
  return slowcat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for i in fun.range(n) do
      _accum_0[_len_0] = _early(Fraction(i - 1, n), pat)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
iter = _patternify(_iter)
_reviter = function(n, pat)
  return slowcat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for i in fun.range(n) do
      _accum_0[_len_0] = _late(Fraction(i - 1, n), pat)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
reviter = _patternify(_reviter)
_segment = function(n, pat)
  return fast(n, pure(id)):appLeft(pat)
end
segment = _patternify(_segment)
_range = function(min, max, pat)
  return pat:fmap(function(x)
    return x * (max - min) + min
  end)
end
range = _patternify_p_p(_range)
superimpose = function(f, pat)
  return stack(pat, f(pat))
end
layer = function(table, pat)
  return stack((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #table do
      local f = table[_index_0]
      _accum_0[_len_0] = f(pat)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
_off = function(time_pat, f, pat)
  return stack(pat, f(late(time_pat, pat)))
end
off = _patternify_p_p(_off)
_echoWith = function(times, time, func, pat)
  local f
  f = function(index)
    return func(_late(time * index, pat))
  end
  local ts
  do
    local _accum_0 = { }
    local _len_0 = 1
    for i = 0, times - 1 do
      _accum_0[_len_0] = i
      _len_0 = _len_0 + 1
    end
    ts = _accum_0
  end
  return stack(map(f, ts))
end
echoWith = _patternify_p_p_p(_echoWith)
_when = function(bool, f, pat)
  return bool and f(pat) or pat
end
when_ = _patternify_p_p(_when)
_firstOf = function(n, f, pat)
  local boolpat = fastcat(concat({
    true
  }, (function()
    local _accum_0 = { }
    local _len_0 = 1
    for i = 1, n - 1 do
      _accum_0[_len_0] = false
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)()))
  return when_(boolpat, f, pat)
end
firstOf = _patternify_p_p(_firstOf)
every = firstOf
_lastOf = function(n, f, pat)
  local boolpat = fastcat(concat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for i = 1, n - 1 do
      _accum_0[_len_0] = false
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), {
    true
  }))
  return when_(boolpat, f, pat)
end
lastOf = _patternify_p_p(_lastOf)
_jux = function(f, pat)
  return _juxBy(0.5, f, pat)
end
jux = _patternify(_jux)
_juxBy = function(by, f, pat)
  by = by / 2
  local elem_or
  elem_or = function(dict, key, default)
    if dict[key] ~= nil then
      return dict[key]
    end
    return default
  end
  local left = pat:fmap(function(valmap)
    return union({
      pan = elem_or(valmap, "pan", 0.5) - by
    }, valmap)
  end)
  local right = pat:fmap(function(valmap)
    return union({
      pan = elem_or(valmap, "pan", 0.5) + by
    }, valmap)
  end)
  return stack(left, f(right))
end
juxBy = _patternify_p_p(_juxBy)
_striate = function(n, pat)
  local ranges
  do
    local _accum_0 = { }
    local _len_0 = 1
    for i = 0, n - 1 do
      _accum_0[_len_0] = {
        begin = i / n,
        ["end"] = (i + 1) / n
      }
      _len_0 = _len_0 + 1
    end
    ranges = _accum_0
  end
  local merge_sample
  merge_sample = function(range)
    local f
    f = function(v)
      return union(range, {
        sound = v.sound
      })
    end
    return pat:fmap(f)
  end
  return fastcat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #ranges do
      local r = ranges[_index_0]
      _accum_0[_len_0] = merge_sample(r)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
striate = _patternify(_striate)
_chop = function(n, pat)
  local ranges
  do
    local _accum_0 = { }
    local _len_0 = 1
    for i = 0, n - 1 do
      _accum_0[_len_0] = {
        begin = i / n,
        ["end"] = (i + 1) / n
      }
      _len_0 = _len_0 + 1
    end
    ranges = _accum_0
  end
  local func
  func = function(o)
    local f
    f = function(slice)
      return union(slice, o)
    end
    return fastcat(map(f, ranges))
  end
  return pat:squeezeBind(func)
end
chop = _patternify(_chop)
slice = function(npat, ipat, opat)
  return npat:innerBind(function(n)
    return ipat:outerBind(function(i)
      return opat:outerBind(function(o)
        local begin
        if type(n) == table then
          begin = n[i]
        else
          begin = i / n
        end
        local _end
        if type(n) == table then
          _end = n[i + 1]
        else
          _end = (i + 1) / n
        end
        return pure(union(o, {
          begin = begin,
          ["end"] = _end,
          _slices = n
        }))
      end)
    end)
  end)
end
splice = function(npat, ipat, opat)
  local sliced = slice(npat, ipat, opat)
  return sliced:withEvent(function(event)
    return event:withValue(function(value)
      local new_attri = {
        speed = tofloat(tofrac(1) / tofrac(value._slices) / event.whole:duration()) * (value.speed or 1),
        unit = "c"
      }
      return union(new_attri, value)
    end)
  end)
end
_loopAt = function(factor, pat)
  pat = pat .. C.speed(1 / factor) .. C.unit("c")
  return slow(factor, pat)
end
loopAt = _patternify(_loopAt)
fit = function(pat)
  return pat:withEvent(function(event)
    return event:withValue(function(value)
      return union(value, {
        speed = tofrac(1) / event.whole:duration(),
        unit = "c"
      })
    end)
  end)
end
_legato = function(factor, pat)
  factor = tofrac(factor)
  return pat:withEventSpan(function(span)
    return Span(span._begin, (span._begin + span:duration() * factor))
  end)
end
legato = _patternify(_legato)
_scale = function(name, pat)
  local toScale
  toScale = function(v)
    return getScale(name, v)
  end
  return pat:fmap(toScale)
end
scale = _patternify(_scale)
apply = function(x, pat)
  return pat .. x
end
sl = string_lambda
print(mini("hh hh@2"))
return {
  C = C,
  Pattern = Pattern,
  id = id,
  pure = pure,
  silence = silence,
  mini = mini,
  reify = reify,
  sl = sl,
  apply = apply,
  run = run,
  scan = scan,
  fastcat = fastcat,
  slowcat = slowcat,
  timecat = timecat,
  randcat = randcat,
  struct = struct,
  euclid = euclid,
  stack = stack,
  layer = layer,
  superimpose = superimpose,
  jux = jux,
  juxBy = juxBy,
  inside = inside,
  outside = outside,
  firstOf = firstOf,
  lastOf = lastOf,
  every = every,
  rev = rev,
  off = off,
  when_ = when_,
  fast = fast,
  slow = slow,
  ply = ply,
  early = early,
  late = late,
  fastgap = fastgap,
  compress = compress,
  zoom = zoom,
  focus = focus,
  degrade = degrade,
  degradeBy = degradeBy,
  undegradeBy = undegradeBy,
  undegrade = undegrade,
  sometimes = sometimes,
  iter = iter,
  reviter = reviter,
  striate = striate,
  chop = chop,
  slice = slice,
  splice = splice,
  loopAt = loopAt,
  scale = scale,
  sine = sine,
  sine2 = sine2,
  square = square,
  square2 = square2,
  saw = saw,
  saw2 = saw2,
  isaw = isaw,
  isaw2 = isaw2,
  tri = tri,
  tri2 = tri2,
  rand = rand,
  irand = irand
}
