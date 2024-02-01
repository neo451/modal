local map, filter, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type
do
  local _obj_0 = require("xi.utils")
  map, filter, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type = _obj_0.map, _obj_0.filter, _obj_0.reduce, _obj_0.id, _obj_0.flatten, _obj_0.totable, _obj_0.dump, _obj_0.concat, _obj_0.rotate, _obj_0.union, _obj_0.timeToRand, _obj_0.curry, _obj_0.type
end
local bjork
bjork = require("xi.theory.euclid").bjork
local parseChord
parseChord = require("xi.theory.chords").parseChord
local getScale
getScale = require("xi.theory.scales").getScale
local Fraction, tofrac, tofloat
do
  local _obj_0 = require("xi.fraction")
  Fraction, tofrac, tofloat = _obj_0.Fraction, _obj_0.tofrac, _obj_0.tofloat
end
local genericParams, aliasParams
do
  local _obj_0 = require("xi.control")
  genericParams, aliasParams = _obj_0.genericParams, _obj_0.aliasParams
end
local Event, Span, State
do
  local _obj_0 = require("xi.types")
  Event, Span, State = _obj_0.Event, _obj_0.Span, _obj_0.State
end
local visit
visit = require("xi.mini.visitor").visit
local op
op = require("fun").op
local fun, C, create, notemt, Interpreter, Pattern, silence, pure, mini, reify, stack, slowcatPrime, slowcat, fastcat, timecat, _fast, _slow, _early, _late, _inside, _outside, waveform, steady, toBipolar, fromBipolar, sine2, sine, cosine2, cosine, square, square2, isaw, isaw2, saw, saw2, tri, tri2, time, rand, _irand, irand, run, scan, _chooseWith, chooseWith, choose, chooseCycles, randcat, polyrhythm, _patternify, _patternify_p_p, _patternify_p_p_p, _off, struct, _euclid, _juxBy, _jux, superimpose, layer, rev, palindrome, _iter, _reviter, _segment, _range, _fastgap, _compress, _degradeByWith, _degradeBy, _undegradeBy, degrade, undegrade, sometimesBy, sometimes, _when, _firstOf, _lastOf, _scale, scale, fastgap, degradeBy, segment, fast, slow, iter, reviter, early, late, inside, outside, when_, firstOf, lastOf, every, off, range, compress, euclid, jux, juxBy, apply, patOfPats, expectedEvents, actualEvents
fun = require("fun")
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
for _index_0 = 1, #genericParams do
  local name = genericParams[_index_0]
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
    eval = function(self, node)
      local tag = node.type
      local method = self[tag]
      return method(self, node)
    end,
    sequence = function(self, node)
      return self:_sequence_elements(node.elements)
    end,
    _sequence_elements = function(self, elements)
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #elements do
          local e = elements[_index_0]
          _accum_0[_len_0] = self:eval(e)
          _len_0 = _len_0 + 1
        end
        elements = _accum_0
      end
      local tc_args = { }
      for _index_0 = 1, #elements do
        local es = elements[_index_0]
        local weight = es[1][1] or 1
        local deg_ratio = es[1][3] or 0
        local pats
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _index_1 = 1, #es do
            local e = es[_index_1]
            _accum_0[_len_0] = e[2]
            _len_0 = _len_0 + 1
          end
          pats = _accum_0
        end
        table.insert(tc_args, {
          #es * weight,
          degradeBy(deg_ratio, fastcat(pats))
        })
      end
      return timecat(tc_args)
    end,
    random_sequence = function(self, node)
      local seqs
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = node.elements
        for _index_0 = 1, #_list_0 do
          local e = _list_0[_index_0]
          _accum_0[_len_0] = self:eval(e)
          _len_0 = _len_0 + 1
        end
        seqs = _accum_0
      end
      return randcat(seqs)
    end,
    polyrhythm = function(self, node)
      local seqs
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = node.seqs
        for _index_0 = 1, #_list_0 do
          local seq = _list_0[_index_0]
          _accum_0[_len_0] = self:eval(seq)
          _len_0 = _len_0 + 1
        end
        seqs = _accum_0
      end
      return polyrhythm(seqs)
    end,
    polymeter = function(self, node)
      local fast_params
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = node.seqs
        for _index_0 = 1, #_list_0 do
          local seq = _list_0[_index_0]
          _accum_0[_len_0] = Fraction(node.steps, #seq.elements)
          _len_0 = _len_0 + 1
        end
        fast_params = _accum_0
      end
      return stack((function()
        local _accum_0 = { }
        local _len_0 = 1
        for _, seq, fp in fun.zip(node.seqs, fast_params) do
          _accum_0[_len_0] = _fast(fp, self:eval(seq))
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)())
    end,
    element = function(self, node)
      local modifiers
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = node.modifiers
        for _index_0 = 1, #_list_0 do
          local mod = _list_0[_index_0]
          _accum_0[_len_0] = self:eval(mod)
          _len_0 = _len_0 + 1
        end
        modifiers = _accum_0
      end
      local pat = self:eval(node.value)
      if node.euclid_modifier ~= nil then
        local k, n, rotation = self:eval(node.euclid_modifier)
        pat = euclid(k, n, rotation, pat)
      end
      local values = {
        {
          1,
          pat,
          0
        }
      }
      for _index_0 = 1, #modifiers do
        local modifier = modifiers[_index_0]
        local n_values
        for _index_1 = 1, #values do
          local v = values[_index_1]
          n_values = modifier(v)
        end
        values = n_values
      end
      return values
    end,
    euclid_modifier = function(self, node)
      local k = self:eval(node.k)
      local n = self:eval(node.n)
      local rotation = nil
      if node.rotation ~= nil then
        rotation = self:eval(node.rotation)
      else
        rotation = pure(0)
      end
      return k, n, rotation
    end,
    modifier = function(self, node)
      local _exp_0 = node.op
      if "fast" == _exp_0 then
        local param = self:_sequence_elements({
          node.value
        })
        return function(w_p)
          return {
            {
              w_p[1],
              fast(param, w_p[2]),
              w_p[3]
            }
          }
        end
      elseif "slow" == _exp_0 then
        local param = self:_sequence_elements({
          node.value
        })
        return function(w_p)
          return {
            {
              w_p[1],
              slow(param, w_p[2]),
              w_p[3]
            }
          }
        end
      elseif "repeat" == _exp_0 then
        return function(w_p)
          local _accum_0 = { }
          local _len_0 = 1
          for i = 1, node.count + 1 do
            _accum_0[_len_0] = w_p
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end
      elseif "weight" == _exp_0 then
        return function(w_p)
          return {
            {
              node.value,
              w_p[2],
              w_p[3]
            }
          }
        end
      elseif "degrade" == _exp_0 then
        local arg = node.value
        local _exp_1 = arg.op
        if "count" == _exp_1 then
          return function(w_p)
            return {
              {
                w_p[1],
                w_p[2],
                Fraction(arg.value, arg.value + 1)
              }
            }
          end
        elseif "value" == _exp_1 then
          return function(w_p)
            return {
              {
                w_p[1],
                w_p[2],
                arg.value
              }
            }
          end
        end
      end
      return function(w_p)
        return {
          {
            w_p[1],
            w_p[2],
            w_p[3]
          }
        }
      end
    end,
    number = function(self, node)
      return pure(node.value)
    end,
    word = function(self, node)
      if node.index ~= 0 then
        return C.sound(node.value):combineLeft(C.n(node.index))
      end
      return pure(node.value)
    end,
    rest = function(self, node)
      return silence()
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function() end,
    __base = _base_0,
    __name = "Interpreter"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Interpreter = _class_0
end
do
  local _class_0
  local _base_0 = {
    querySpan = function(self, b, e)
      local span = Span(b, e)
      local state = State(span)
      return self:query(state)
    end,
    firstCycle = function(self)
      return self:querySpan(0, 1)
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
          local events = func(a.value):querySpan(a.part._begin, a.part._end)
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
    withTime = function(self, q_func, e_func)
      local query = self:withQueryTime(q_func)
      local pattern = query:withEventTime(e_func)
      return pattern
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
    appLeft = function(self, pat_val)
      local query
      query = function(_, state)
        local events = { }
        local event_funcs = self:query(state)
        for _index_0 = 1, #event_funcs do
          local event_func = event_funcs[_index_0]
          local whole = event_func:wholeOrPart()
          local event_vals = pat_val:querySpan(whole._begin, whole._end)
          for _index_1 = 1, #event_vals do
            local event_val = event_vals[_index_1]
            local new_whole = event_func.whole
            local new_part = event_func.part:sect(event_val.part)
            local new_context = event_val:combineContext(event_func)
            if new_part ~= nil then
              local new_value = event_func.value(event_val.value)
              table.insert(events, Event(new_whole, new_part, new_value))
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
          local event_funcs = self:querySpan(whole._begin, whole._end)
          event_val:wholeOrPart()
          for _index_1 = 1, #event_funcs do
            local event_func = event_funcs[_index_1]
            local new_whole = event_val.whole
            local new_part = event_func.part:sect(event_val.part)
            local new_context = event_val:combineContext(event_func)
            if new_part ~= nil then
              local new_value = event_func.value(event_val.value)
              table.insert(events, Event(new_whole, new_part, new_value))
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
mini = function(string)
  local ast = visit(string)
  return Interpreter:eval(ast)
end
reify = function(thing)
  local _exp_0 = type(thing)
  if "string" == _exp_0 then
    return mini(thing)
  elseif "table" == _exp_0 then
    return fastcat(thing)
  elseif "pattern" == _exp_0 then
    return thing
  else
    return pure(thing)
  end
end
stack = function(...)
  local pats = map(reify, totable(...))
  local query
  query = function(self, state)
    local span = state.span
    local f
    f = function(pat)
      return pat:querySpan(span._begin, span._end)
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
    table.insert(pats, new_pat)
    accum = accum + time
  end
  return stack(pats)
end
_fast = function(rate, pat)
  return (reify(pat)):withTime((function(t)
    return t * rate
  end), (function(t)
    return t / rate
  end))
end
_slow = function(factor, pat)
  return _fast((1 / factor), pat)
end
_early = function(offset, pat)
  return (reify(pat)):withTime((function(t)
    return t + offset
  end), (function(t)
    return t - offset
  end))
end
_late = function(offset, pat)
  return _early(-offset, pat)
end
_inside = function(factor, f, pat)
  return _fast(factor, f(_slow(factor, pat)))
end
_outside = function(factor, f, pat)
  return _inside(1 / factor, f, pat)
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
  return new(Pattern(function(state)
    return {
      Event(nil, state.span, value)
    }
  end))
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
  return math.sin(t:asFloat() * math.pi * 2)
end)
sine = fromBipolar(sine2)
cosine2 = _late(1 / 4, sine2)
cosine = fromBipolar(cosine2)
square = waveform(function(t)
  return math.floor((t * 2) % 2)
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
    return math.floor(x * i)
  end)
end
irand = function(ipat)
  return reify(ipat):fmap(_irand):innerJoin()
end
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
_chooseWith = function(pat, ...)
  local vals = map(reify, totable(...))
  if #vals == 0 then
    return silence()
  end
  return range(1, #vals + 1, pat):fmap(function(i)
    local key = math.min(math.max(math.floor(i), 0), #vals)
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
    patOfPats = apat:fmap(mapFn):appLeft(bpat)
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
    patOfPats = apat:fmap(mapFn):appLeft(bpat):appLeft(cpat)
    return patOfPats:innerJoin()
  end
  return patterned
end
_off = function(time_pat, f, pat)
  return stack(pat, f(late(time_pat, pat)))
end
struct = function(boolpat, pat)
  pat, boolpat = reify(pat), reify(boolpat)
  return boolpat:fmap(function(b)
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
_jux = function(f, pat)
  return _juxBy(0.5, f, pat)
end
superimpose = function(f, pat)
  return stack(pat, f(pat))
end
layer = function(table, pat)
  return stack((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #table do
      local f = table[_index_0]
      _accum_0[_len_0] = f(reify(pat))
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
rev = function(pat)
  pat = reify(pat)
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
      _accum_0[_len_0] = _early(Fraction(i - 1, n), reify(pat))
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
_reviter = function(n, pat)
  return slowcat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for i in fun.range(n) do
      _accum_0[_len_0] = _late(Fraction(i - 1, n), reify(pat))
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)())
end
_segment = function(n, pat)
  return fast(n, pure(id)):appLeft(pat)
end
_range = function(min, max, pat)
  return pat:fmap(function(x)
    return x * (max - min) + min
  end)
end
_fastgap = function(factor, pat)
  pat = reify(pat)
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
_compress = function(b, e, pat)
  pat = reify(pat)
  b, e = tofrac(b), tofrac(e)
  if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0) then
    return silence()
  end
  local fasted = _fastgap((Fraction(1) / (e - b)), pat)
  return _late(b, fasted)
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
_when = function(bool, f, pat)
  return bool and f(pat) or pat
end
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
_scale = function(name, pat)
  pat = reify(pat)
  local toScale
  toScale = function(v)
    return getScale(name, v)
  end
  return pat:fmap(toScale)
end
scale = _patternify(_scale)
fastgap = _patternify(_fastgap)
degradeBy = _patternify(_degradeBy)
segment = _patternify(_segment)
fast = _patternify(_fast)
slow = _patternify(_slow)
iter = _patternify(_iter)
reviter = _patternify(_reviter)
early = _patternify(_early)
late = _patternify(_late)
inside = _patternify_p_p(_inside)
outside = _patternify_p_p(_outside)
when_ = _patternify_p_p(_when)
firstOf = _patternify_p_p(_firstOf)
lastOf = _patternify_p_p(_lastOf)
every = firstOf
off = _patternify_p_p(_off)
range = _patternify_p_p(_range)
compress = _patternify_p_p(_compress)
euclid = _patternify_p_p_p(_euclid)
jux = _patternify(_jux)
juxBy = _patternify_p_p(_juxBy)
apply = function(x, pat)
  return pat .. x
end
patOfPats = pure(fastcat("a", "b"))
expectedEvents = {
  Event(Span(0, 1), Span(0, 1 / 2), "a"),
  Event(Span(0, 1), Span(1 / 2, 1), "b")
}
actualEvents = patOfPats:outerJoin():firstCycle()
return {
  when_ = when_,
  C = C,
  Pattern = Pattern,
  apply = apply,
  id = id,
  pure = pure,
  silence = silence,
  mini = mini,
  reify = reify,
  run = run,
  scan = scan,
  fastcat = fastcat,
  slowcat = slowcat,
  timecat = timecat,
  randcat = randcat,
  euclid = euclid,
  stack = stack,
  layer = layer,
  superimpose = superimpose,
  struct = struct,
  jux = jux,
  juxBy = juxBy,
  inside = inside,
  outside = outside,
  firstOf = firstOf,
  lastOf = lastOf,
  every = every,
  rev = rev,
  off = off,
  every = every,
  fast = fast,
  slow = slow,
  early = early,
  late = late,
  fastgap = fastgap,
  compress = compress,
  degrade = degrade,
  degradeBy = degradeBy,
  undegradeBy = undegradeBy,
  undegrade = undegrade,
  sometimes = sometimes,
  iter = iter,
  reviter = reviter,
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
