-- LuLPeg, a pure Lua port of LPeg, Roberto Ierusalimschy's
-- Parsing Expression Grammars library.
--
-- Copyright (C) Pierre-Yves Gerardy.
-- Released under the Romantic WTF Public License (cf. the LICENSE
-- file or the end of this file, whichever is present).
--
-- See http://www.inf.puc-rio.br/~roberto/lpeg/ for the original.
--
-- The re.lua module and the test suite (tests/lpeg.*.*.tests.lua)
-- are part of the original LPeg distribution.
local function lulpeg()
   local _ENV, loaded, packages, release, require_ = _ENV or _G, {}, {}, true, require

   local function require(...)
      local lib = ...

      -- is it a private file?
      if loaded[lib] then
         return loaded[lib]
      elseif packages[lib] then
         loaded[lib] = packages[lib](lib)
         return loaded[lib]
      else
         return require_(lib)
      end
   end

   --=============================================================================
   do
      local _ENV = _ENV
      packages["analizer"] = function(...)
         local u = require "util"
         local nop, weakkey = u.nop, u.weakkey
         local hasVcache, hasCmtcache, lengthcache = weakkey {}, weakkey {}, weakkey {}
         return {
            hasV = nop,
            hasCmt = nop,
            length = nop,
            hasCapture = nop,
         }
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["compiler"] = function(...)
         local assert, error, pairs, print, rawset, select, setmetatable, tostring, type =
            assert, error, pairs, print, rawset, select, setmetatable, tostring, type
         local s, t, u = require "string", require "table", require "util"
         local _ENV = u.noglobals() ----------------------------------------------------
         local s_byte, s_sub, t_concat, t_insert, t_remove, t_unpack =
            s.byte, s.sub, t.concat, t.insert, t.remove, u.unpack
         local load, map, map_all, t_pack = u.load, u.map, u.map_all, u.pack
         local expose = u.expose
         return function(Builder, LL)
            local evaluate, LL_ispattern = LL.evaluate, LL.ispattern
            local charset = Builder.charset
            local compilers = {}
            local function compile(pt, ccache)
               if not LL_ispattern(pt) then
                  error "pattern expected"
               end
               local typ = pt.pkind
               if typ == "grammar" then
                  ccache = {}
               elseif typ == "ref" or typ == "choice" or typ == "sequence" then
                  if not ccache[pt] then
                     ccache[pt] = compilers[typ](pt, ccache)
                  end
                  return ccache[pt]
               end
               if not pt.compiled then
                  pt.compiled = compilers[pt.pkind](pt, ccache)
               end
               return pt.compiled
            end
            LL.compile = compile
            local function clear_captures(ary, ci)
               for i = ci, #ary do
                  ary[i] = nil
               end
            end
            local LL_compile, LL_evaluate, LL_P = LL.compile, LL.evaluate, LL.P
            local function computeidex(i, len)
               if i == 0 or i == 1 or i == nil then
                  return 1
               elseif type(i) ~= "number" then
                  error "number or nil expected for the stating index"
               elseif i > 0 then
                  return i > len and len + 1 or i
               else
                  return len + i < 0 and 1 or len + i + 1
               end
            end
            local function newcaps()
               return {
                  kind = {},
                  bounds = {},
                  openclose = {},
                  -- [[DBG]] dbgcaps
                  aux = {},
               }
            end
            local function _match(dbg, pt, sbj, si, ...)
               if dbg then -------------
                  print("@!!! Match !!!@", pt)
               end ---------------------
               pt = LL_P(pt)
               assert(type(sbj) == "string", "string expected for the match subject")
               si = computeidex(si, #sbj)
               if dbg then -------------
                  print(("-"):rep(30))
                  print(pt.pkind)
                  LL.pprint(pt)
               end ---------------------
               local matcher = compile(pt, {})
               local caps = newcaps()
               local matcher_state = { grammars = {}, args = { n = select("#", ...), ... }, tags = {} }
               local success, final_si, ci = matcher(sbj, si, caps, 1, matcher_state)
               if dbg then -------------
                  print(
                     "!!! Done Matching !!! success: ",
                     success,
                     "final position",
                     final_si,
                     "final cap index",
                     ci,
                     "#caps",
                     #caps.openclose
                  )
               end ----------------------
               if success then
                  clear_captures(caps.kind, ci)
                  clear_captures(caps.aux, ci)
                  if dbg then -------------
                     print("trimmed cap index = ", #caps + 1)
                     LL.cprint(caps, sbj, 1)
                  end ---------------------
                  local values, _, vi = LL_evaluate(caps, sbj, 1, 1)
                  if dbg then -------------
                     print("#values", vi)
                     expose(values)
                  end ---------------------
                  if vi == 0 then
                     return final_si
                  else
                     return t_unpack(values, 1, vi)
                  end
               else
                  if dbg then
                     print "Failed"
                  end
                  return nil
               end
            end
            function LL.match(...)
               return _match(false, ...)
            end
            function LL.dmatch(...)
               return _match(true, ...)
            end
            for _, v in pairs {
               "C",
               "Cf",
               "Cg",
               "Cs",
               "Ct",
               "Clb",
               "div_string",
               "div_table",
               "div_number",
               "div_function",
            } do
               compilers[v] = load(
                  ([=[
    local compile, expose, type, LL = ...
    return function (pt, ccache)
        local matcher, this_aux = compile(pt.pattern, ccache), pt.aux
        return function (sbj, si, caps, ci, state)
            local ref_ci = ci
            local kind, bounds, openclose, aux 
                = caps.kind, caps.bounds, caps.openclose, caps.aux
            kind      [ci] = "XXXX"
            bounds    [ci] = si
            openclose [ci] = 0
            caps.aux       [ci] = (this_aux or false)
            local success
            success, si, ci
                = matcher(sbj, si, caps, ci + 1, state)
            if success then
                if ci == ref_ci + 1 then
                    caps.openclose[ref_ci] = si
                else
                    kind      [ci] = "XXXX"
                    bounds    [ci] = si
                    openclose [ci] = ref_ci - ci
                    aux       [ci] = this_aux or false
                    ci = ci + 1
                end
            else
                ci = ci - 1
            end
            return success, si, ci
        end
    end]=]):gsub("XXXX", v),
                  v .. " compiler"
               )(compile, expose, type, LL)
            end
            compilers["Carg"] = function(pt, ccache)
               local n = pt.aux
               return function(sbj, si, caps, ci, state)
                  if state.args.n < n then
                     error("reference to absent argument #" .. n)
                  end
                  caps.kind[ci] = "value"
                  caps.bounds[ci] = si
                  if state.args[n] == nil then
                     caps.openclose[ci] = 1 / 0
                     caps.aux[ci] = 1 / 0
                  else
                     caps.openclose[ci] = si
                     caps.aux[ci] = state.args[n]
                  end
                  return true, si, ci + 1
               end
            end
            for _, v in pairs {
               "Cb",
               "Cc",
               "Cp",
            } do
               compilers[v] = load(
                  ([=[
    return function (pt, ccache)
        local this_aux = pt.aux
        return function (sbj, si, caps, ci, state)
            caps.kind      [ci] = "XXXX"
            caps.bounds    [ci] = si
            caps.openclose [ci] = si
            caps.aux       [ci] = this_aux or false
            return true, si, ci + 1
        end
    end]=]):gsub("XXXX", v),
                  v .. " compiler"
               )(expose)
            end
            compilers["/zero"] = function(pt, ccache)
               local matcher = compile(pt.pattern, ccache)
               return function(sbj, si, caps, ci, state)
                  local success, nsi = matcher(sbj, si, caps, ci, state)
                  clear_captures(caps.aux, ci)
                  return success, nsi, ci
               end
            end
            local function pack_Cmt_caps(i, ...)
               return i, t_pack(...)
            end
            compilers["Cmt"] = function(pt, ccache)
               local matcher, func = compile(pt.pattern, ccache), pt.aux
               return function(sbj, si, caps, ci, state)
                  local success, Cmt_si, Cmt_ci = matcher(sbj, si, caps, ci, state)
                  if not success then
                     clear_captures(caps.aux, ci)
                     return false, si, ci
                  end
                  local final_si, values
                  if Cmt_ci == ci then
                     final_si, values = pack_Cmt_caps(func(sbj, Cmt_si, s_sub(sbj, si, Cmt_si - 1)))
                  else
                     clear_captures(caps.aux, Cmt_ci)
                     clear_captures(caps.kind, Cmt_ci)
                     local cps, _, nn = evaluate(caps, sbj, ci)
                     final_si, values = pack_Cmt_caps(func(sbj, Cmt_si, t_unpack(cps, 1, nn)))
                  end
                  if not final_si then
                     return false, si, ci
                  end
                  if final_si == true then
                     final_si = Cmt_si
                  end
                  if type(final_si) == "number" and si <= final_si and final_si <= #sbj + 1 then
                     local kind, bounds, openclose, aux = caps.kind, caps.bounds, caps.openclose, caps.aux
                     for i = 1, values.n do
                        kind[ci] = "value"
                        bounds[ci] = si
                        if values[i] == nil then
                           caps.openclose[ci] = 1 / 0
                           caps.aux[ci] = 1 / 0
                        else
                           caps.openclose[ci] = final_si
                           caps.aux[ci] = values[i]
                        end
                        ci = ci + 1
                     end
                  elseif type(final_si) == "number" then
                     error "Index out of bounds returned by match-time capture."
                  else
                     error(
                        "Match time capture must return a number, a boolean or nil"
                           .. " as first argument, or nothing at all."
                     )
                  end
                  return true, final_si, ci
               end
            end
            compilers["string"] = function(pt, ccache)
               local S = pt.aux
               local N = #S
               return function(sbj, si, caps, ci, state)
                  local in_1 = si - 1
                  for i = 1, N do
                     local c
                     c = s_byte(sbj, in_1 + i)
                     if c ~= S[i] then
                        return false, si, ci
                     end
                  end
                  return true, si + N, ci
               end
            end
            compilers["char"] = function(pt, ccache)
               return load(([=[
        local s_byte, s_char = ...
        return function(sbj, si, caps, ci, state)
            local c, nsi = s_byte(sbj, si), si + 1
            if c ~= __C0__ then
                return false, si, ci
            end
            return true, nsi, ci
        end]=]):gsub("__C0__", tostring(pt.aux)))(s_byte, ("").char)
            end
            local function truecompiled(sbj, si, caps, ci, state)
               return true, si, ci
            end
            compilers["true"] = function(pt)
               return truecompiled
            end
            local function falsecompiled(sbj, si, caps, ci, state)
               return false, si, ci
            end
            compilers["false"] = function(pt)
               return falsecompiled
            end
            local function eoscompiled(sbj, si, caps, ci, state)
               return si > #sbj, si, ci
            end
            compilers["eos"] = function(pt)
               return eoscompiled
            end
            local function onecompiled(sbj, si, caps, ci, state)
               local char, _ = s_byte(sbj, si), si + 1
               if char then
                  return true, si + 1, ci
               else
                  return false, si, ci
               end
            end
            compilers["one"] = function(pt)
               return onecompiled
            end
            compilers["any"] = function(pt)
               local N = pt.aux
               if N == 1 then
                  return onecompiled
               else
                  N = pt.aux - 1
                  return function(sbj, si, caps, ci, state)
                     local n = si + N
                     if n <= #sbj then
                        return true, n + 1, ci
                     else
                        return false, si, ci
                     end
                  end
               end
            end
            do
               local function checkpatterns(g)
                  for k, v in pairs(g.aux) do
                     if not LL_ispattern(v) then
                        error(("rule 'A' is not a pattern"):gsub("A", tostring(k)))
                     end
                  end
               end
               compilers["grammar"] = function(pt, ccache)
                  checkpatterns(pt)
                  local gram = map_all(pt.aux, compile, ccache)
                  local start = gram[1]
                  return function(sbj, si, caps, ci, state)
                     t_insert(state.grammars, gram)
                     local success, nsi, ci = start(sbj, si, caps, ci, state)
                     t_remove(state.grammars)
                     return success, nsi, ci
                  end
               end
            end
            local dummy_acc = { kind = {}, bounds = {}, openclose = {}, aux = {} }
            compilers["behind"] = function(pt, ccache)
               local matcher, N = compile(pt.pattern, ccache), pt.aux
               return function(sbj, si, caps, ci, state)
                  if si <= N then
                     return false, si, ci
                  end
                  local success = matcher(sbj, si - N, dummy_acc, ci, state)
                  dummy_acc.aux = {}
                  return success, si, ci
               end
            end
            compilers["range"] = function(pt)
               local ranges = pt.aux
               return function(sbj, si, caps, ci, state)
                  local char, nsi = s_byte(sbj, si), si + 1
                  for i = 1, #ranges do
                     local r = ranges[i]
                     if char and r[char] then
                        return true, nsi, ci
                     end
                  end
                  return false, si, ci
               end
            end
            compilers["set"] = function(pt)
               local s = pt.aux
               return function(sbj, si, caps, ci, state)
                  local char, nsi = s_byte(sbj, si), si + 1
                  if s[char] then
                     return true, nsi, ci
                  else
                     return false, si, ci
                  end
               end
            end
            compilers["range"] = compilers.set
            compilers["ref"] = function(pt, ccache)
               local name = pt.aux
               local ref
               return function(sbj, si, caps, ci, state)
                  if not ref then
                     if #state.grammars == 0 then
                        error(("rule 'XXXX' used outside a grammar"):gsub("XXXX", tostring(name)))
                     elseif not state.grammars[#state.grammars][name] then
                        error(("rule 'XXXX' undefined in given grammar"):gsub("XXXX", tostring(name)))
                     end
                     ref = state.grammars[#state.grammars][name]
                  end
                  local success, nsi, nci = ref(sbj, si, caps, ci, state)
                  return success, nsi, nci
               end
            end
            local choice_tpl = [=[
            success, si, ci = XXXX(sbj, si, caps, ci, state)
            if success then
                return true, si, ci
            else
            end]=]
            local function flatten(kind, pt, ccache)
               if pt[2].pkind == kind then
                  return compile(pt[1], ccache), flatten(kind, pt[2], ccache)
               else
                  return compile(pt[1], ccache), compile(pt[2], ccache)
               end
            end
            compilers["choice"] = function(pt, ccache)
               local choices = { flatten("choice", pt, ccache) }
               local names, chunks = {}, {}
               for i = 1, #choices do
                  local m = "ch" .. i
                  names[#names + 1] = m
                  chunks[#names] = choice_tpl:gsub("XXXX", m)
               end
               names[#names + 1] = "clear_captures"
               choices[#names] = clear_captures
               local compiled = t_concat {
                  "local ",
                  t_concat(names, ", "),
                  [=[ = ...
        return function (sbj, si, caps, ci, state)
            local aux, success = caps.aux, false
            ]=],
                  t_concat(chunks, "\n"),
                  [=[--
            return false, si, ci
        end]=],
               }
               return load(compiled, "Choice")(t_unpack(choices))
            end
            local sequence_tpl = [=[
            success, si, ci = XXXX(sbj, si, caps, ci, state)
            if not success then
                return false, ref_si, ref_ci
            end]=]
            compilers["sequence"] = function(pt, ccache)
               local sequence = { flatten("sequence", pt, ccache) }
               local names, chunks = {}, {}
               for i = 1, #sequence do
                  local m = "seq" .. i
                  names[#names + 1] = m
                  chunks[#names] = sequence_tpl:gsub("XXXX", m)
               end
               names[#names + 1] = "clear_captures"
               sequence[#names] = clear_captures
               local compiled = t_concat {
                  "local ",
                  t_concat(names, ", "),
                  [=[ = ...
        return function (sbj, si, caps, ci, state)
            local ref_si, ref_ci, success = si, ci
            ]=],
                  t_concat(chunks, "\n"),
                  [=[
            return true, si, ci
        end]=],
               }
               return load(compiled, "Sequence")(t_unpack(sequence))
            end
            compilers["at most"] = function(pt, ccache)
               local matcher, n = compile(pt.pattern, ccache), pt.aux
               n = -n
               return function(sbj, si, caps, ci, state)
                  local success = true
                  for i = 1, n do
                     success, si, ci = matcher(sbj, si, caps, ci, state)
                     if not success then
                        break
                     end
                  end
                  return true, si, ci
               end
            end
            compilers["at least"] = function(pt, ccache)
               local matcher, n = compile(pt.pattern, ccache), pt.aux
               if n == 0 then
                  return function(sbj, si, caps, ci, state)
                     local last_si, last_ci
                     while true do
                        local success
                        last_si, last_ci = si, ci
                        success, si, ci = matcher(sbj, si, caps, ci, state)
                        if not success then
                           si, ci = last_si, last_ci
                           break
                        end
                     end
                     return true, si, ci
                  end
               elseif n == 1 then
                  return function(sbj, si, caps, ci, state)
                     local last_si, last_ci
                     local success = true
                     success, si, ci = matcher(sbj, si, caps, ci, state)
                     if not success then
                        return false, si, ci
                     end
                     while true do
                        local success
                        last_si, last_ci = si, ci
                        success, si, ci = matcher(sbj, si, caps, ci, state)
                        if not success then
                           si, ci = last_si, last_ci
                           break
                        end
                     end
                     return true, si, ci
                  end
               else
                  return function(sbj, si, caps, ci, state)
                     local last_si, last_ci
                     local success = true
                     for _ = 1, n do
                        success, si, ci = matcher(sbj, si, caps, ci, state)
                        if not success then
                           return false, si, ci
                        end
                     end
                     while true do
                        local success
                        last_si, last_ci = si, ci
                        success, si, ci = matcher(sbj, si, caps, ci, state)
                        if not success then
                           si, ci = last_si, last_ci
                           break
                        end
                     end
                     return true, si, ci
                  end
               end
            end
            compilers["unm"] = function(pt, ccache)
               if pt.pkind == "any" and pt.aux == 1 then
                  return eoscompiled
               end
               local matcher = compile(pt.pattern, ccache)
               return function(sbj, si, caps, ci, state)
                  local success, _, _ = matcher(sbj, si, caps, ci, state)
                  return not success, si, ci
               end
            end
            compilers["lookahead"] = function(pt, ccache)
               local matcher = compile(pt.pattern, ccache)
               return function(sbj, si, caps, ci, state)
                  local success, _, _ = matcher(sbj, si, caps, ci, state)
                  return success, si, ci
               end
            end
         end
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["datastructures"] = function(...)
         local getmetatable, pairs, setmetatable, type = getmetatable, pairs, setmetatable, type
         local m, t, u = require "math", require "table", require "util"
         local compat = require "compat"
         local ffi
         if compat.luajit then
            ffi = require "ffi"
         end
         local _ENV = u.noglobals() ----------------------------------------------------
         local extend, load, u_max = u.extend, u.load, u.max
         local m_max, t_concat, t_insert, t_sort = m.max, t.concat, t.insert, t.sort
         local structfor = {}
         local byteset_new, isboolset, isbyteset
         local byteset_mt = {}
         local function byteset_constructor(upper)
            local set = setmetatable(
               load(t_concat {
                  "return{ [0]=false",
                  (", false"):rep(upper),
                  " }",
               })(),
               byteset_mt
            )
            return set
         end
         if compat.jit then
            local struct, boolset_constructor = { v = {} }
            function byteset_mt.__index(s, i)
               if i == nil or i > s.upper then
                  return nil
               end
               return s.v[i]
            end
            function byteset_mt.__len(s)
               return s.upper
            end
            function byteset_mt.__newindex(s, i, v)
               s.v[i] = v
            end
            boolset_constructor = ffi.metatype("struct { int upper; bool v[?]; }", byteset_mt)
            function byteset_new(t)
               if type(t) == "number" then
                  local res = boolset_constructor(t + 1)
                  res.upper = t
                  return res
               end
               local upper = u_max(t)
               struct.upper = upper
               if upper > 255 then
                  error "bool_set overflow"
               end
               local set = boolset_constructor(upper + 1)
               set.upper = upper
               for i = 1, #t do
                  set[t[i]] = true
               end
               return set
            end
            function isboolset(s)
               return type(s) == "cdata" and ffi.istype(s, boolset_constructor)
            end
            isbyteset = isboolset
         else
            function byteset_new(t)
               if type(t) == "number" then
                  return byteset_constructor(t)
               end
               local set = byteset_constructor(u_max(t))
               for i = 1, #t do
                  set[t[i]] = true
               end
               return set
            end
            function isboolset(s)
               return false
            end
            function isbyteset(s)
               return getmetatable(s) == byteset_mt
            end
         end
         local function byterange_new(low, high)
            high = (low <= high) and high or -1
            local set = byteset_new(high)
            for i = low, high do
               set[i] = true
            end
            return set
         end
         local tmpa, tmpb = {}, {}
         local function set_if_not_yet(s, dest)
            if type(s) == "number" then
               dest[s] = true
               return dest
            else
               return s
            end
         end
         local function clean_ab(a, b)
            tmpa[a] = nil
            tmpb[b] = nil
         end
         local function byteset_union(a, b)
            local upper = m_max(type(a) == "number" and a or #a, type(b) == "number" and b or #b)
            local A, B = set_if_not_yet(a, tmpa), set_if_not_yet(b, tmpb)
            local res = byteset_new(upper)
            for i = 0, upper do
               res[i] = A[i] or B[i] or false
            end
            clean_ab(a, b)
            return res
         end
         local function byteset_difference(a, b)
            local res = {}
            for i = 0, 255 do
               res[i] = a[i] and not b[i]
            end
            return res
         end
         local function byteset_tostring(s)
            local list = {}
            for i = 0, 255 do
               list[#list + 1] = (s[i] == true) and i or nil
            end
            return t_concat(list, ", ")
         end
         structfor.binary = {
            set = {
               new = byteset_new,
               union = byteset_union,
               difference = byteset_difference,
               tostring = byteset_tostring,
            },
            Range = byterange_new,
            isboolset = isboolset,
            isbyteset = isbyteset,
            isset = isbyteset,
         }
         local set_mt = {}
         local function set_new(t)
            local set = setmetatable({}, set_mt)
            for i = 1, #t do
               set[t[i]] = true
            end
            return set
         end
         -- helper for the union code.
         local function add_elements(a, res)
            for k in pairs(a) do
               res[k] = true
            end
            return res
         end
         local function set_union(a, b)
            a, b = (type(a) == "number") and set_new { a } or a, (type(b) == "number") and set_new { b } or b
            local res = set_new {}
            add_elements(a, res)
            add_elements(b, res)
            return res
         end
         local function set_difference(a, b)
            local list = {}
            a, b = (type(a) == "number") and set_new { a } or a, (type(b) == "number") and set_new { b } or b
            for el in pairs(a) do
               if a[el] and not b[el] then
                  list[#list + 1] = el
               end
            end
            return set_new(list)
         end
         local function set_tostring(s)
            local list = {}
            for el in pairs(s) do
               t_insert(list, el)
            end
            t_sort(list)
            return t_concat(list, ",")
         end
         local function isset(s)
            return (getmetatable(s) == set_mt)
         end
         local function range_new(start, finish)
            local list = {}
            for i = start, finish do
               list[#list + 1] = i
            end
            return set_new(list)
         end
         structfor.other = {
            set = {
               new = set_new,
               union = set_union,
               tostring = set_tostring,
               difference = set_difference,
            },
            Range = range_new,
            isboolset = isboolset,
            isbyteset = isbyteset,
            isset = isset,
            isrange = function(a)
               return false
            end,
         }
         return function(Builder, LL)
            local cs = (Builder.options or {}).charset or "binary"
            if type(cs) == "string" then
               cs = (cs == "binary") and "binary" or "other"
            else
               cs = cs.binary and "binary" or "other"
            end
            return extend(Builder, structfor[cs])
         end
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["util"] = function(...)
         local getmetatable, setmetatable, load, loadstring, next, pairs, pcall, print, rawget, rawset, select, tostring, type, unpack =
            getmetatable,
            setmetatable,
            load,
            loadstring,
            next,
            pairs,
            pcall,
            print,
            rawget,
            rawset,
            select,
            tostring,
            type,
            unpack
         local m, s, t = require "math", require "string", require "table"
         local m_max, s_match, s_gsub, t_concat, t_insert = m.max, s.match, s.gsub, t.concat, t.insert
         local compat = require "compat"
         local function nop() end
         local noglobals, getglobal, setglobal
         if pcall and not compat.lua52 and not release then
            local function errR(_, i)
               error("illegal global read: " .. tostring(i), 2)
            end
            local function errW(_, i, v)
               error("illegal global write: " .. tostring(i) .. ": " .. tostring(v), 2)
            end
            local env = setmetatable({}, { __index = errR, __newindex = errW })
            noglobals = function()
               pcall(setfenv, 3, env)
            end
            function getglobal(k)
               rawget(env, k)
            end
            function setglobal(k, v)
               rawset(env, k, v)
            end
         else
            noglobals = nop
         end
         local _ENV = noglobals() ------------------------------------------------------
         local util = {
            nop = nop,
            noglobals = noglobals,
            getglobal = getglobal,
            setglobal = setglobal,
         }
         util.unpack = t.unpack or unpack
         util.pack = t.pack or function(...)
            return { n = select("#", ...), ... }
         end
         if compat.lua51 then
            local old_load = load
            function util.load(ld, source, mode, env)
               local fun
               if type(ld) == "string" then
                  fun = loadstring(ld)
               else
                  fun = old_load(ld, source)
               end
               if env then
                  setfenv(fun, env)
               end
               return fun
            end
         else
            util.load = load
         end
         if compat.luajit and compat.jit then
            function util.max(ary)
               local max = 0
               for i = 1, #ary do
                  max = m_max(max, ary[i])
               end
               return max
            end
         elseif compat.luajit then
            local t_unpack = util.unpack
            function util.max(ary)
               local len = #ary
               if len <= 30 or len > 10240 then
                  local max = 0
                  for i = 1, #ary do
                     local j = ary[i]
                     if j > max then
                        max = j
                     end
                  end
                  return max
               else
                  return m_max(t_unpack(ary))
               end
            end
         else
            local t_unpack = util.unpack
            local safe_len = 1000
            function util.max(array)
               local len = #array
               if len == 0 then
                  return -1
               end -- FIXME: shouldn't this be `return -1`?
               local off = 1
               local off_end = safe_len
               local max = array[1] -- seed max.
               repeat
                  if off_end > len then
                     off_end = len
                  end
                  local seg_max = m_max(t_unpack(array, off, off_end))
                  if seg_max > max then
                     max = seg_max
                  end
                  off = off + safe_len
                  off_end = off_end + safe_len
               until off >= len
               return max
            end
         end
         local function setmode(t, mode)
            local mt = getmetatable(t) or {}
            if mt.__mode then
               error("The mode has already been set on table " .. tostring(t) .. ".")
            end
            mt.__mode = mode
            return setmetatable(t, mt)
         end
         util.setmode = setmode
         function util.weakboth(t)
            return setmode(t, "kv")
         end
         function util.weakkey(t)
            return setmode(t, "k")
         end
         function util.weakval(t)
            return setmode(t, "v")
         end
         function util.strip_mt(t)
            return setmetatable(t, nil)
         end
         local getuniqueid
         do
            local N, index = 0, {}
            function getuniqueid(v)
               if not index[v] then
                  N = N + 1
                  index[v] = N
               end
               return index[v]
            end
         end
         util.getuniqueid = getuniqueid
         do
            local counter = 0
            function util.gensym()
               counter = counter + 1
               return "___SYM_" .. counter
            end
         end
         function util.passprint(...)
            print(...)
            return ...
         end
         local val_to_str_, key_to_str, table_tostring, cdata_to_str, t_cache
         local multiplier = 2
         local function val_to_string(v, indent)
            indent = indent or 0
            t_cache = {} -- upvalue.
            local acc = {}
            val_to_str_(v, acc, indent, indent)
            local res = t_concat(acc, "")
            return res
         end
         util.val_to_str = val_to_string
         function val_to_str_(v, acc, indent, str_indent)
            str_indent = str_indent or 1
            if "string" == type(v) then
               v = s_gsub(v, "\n", "\n" .. (" "):rep(indent * multiplier + str_indent))
               if s_match(s_gsub(v, "[^'\"]", ""), '^"+$') then
                  acc[#acc + 1] = t_concat { "'", "", v, "'" }
               else
                  acc[#acc + 1] = t_concat { '"', s_gsub(v, '"', '\\"'), '"' }
               end
            elseif "cdata" == type(v) then
               cdata_to_str(v, acc, indent)
            elseif "table" == type(v) then
               if t_cache[v] then
                  acc[#acc + 1] = t_cache[v]
               else
                  t_cache[v] = tostring(v)
                  table_tostring(v, acc, indent)
               end
            else
               acc[#acc + 1] = tostring(v)
            end
         end
         function key_to_str(k, acc, indent)
            if "string" == type(k) and s_match(k, "^[_%a][_%a%d]*$") then
               acc[#acc + 1] = s_gsub(k, "\n", (" "):rep(indent * multiplier + 1) .. "\n")
            else
               acc[#acc + 1] = "[ "
               val_to_str_(k, acc, indent)
               acc[#acc + 1] = " ]"
            end
         end
         function cdata_to_str(v, acc, indent)
            acc[#acc + 1] = (" "):rep(indent * multiplier)
            acc[#acc + 1] = "["
            print(#acc)
            for i = 0, #v do
               if i % 16 == 0 and i ~= 0 then
                  acc[#acc + 1] = "\n"
                  acc[#acc + 1] = (" "):rep(indent * multiplier + 2)
               end
               acc[#acc + 1] = v[i] and 1 or 0
               acc[#acc + 1] = i ~= #v and ", " or ""
            end
            print(#acc, acc[1], acc[2])
            acc[#acc + 1] = "]"
         end
         function table_tostring(tbl, acc, indent)
            acc[#acc + 1] = t_cache[tbl]
            acc[#acc + 1] = "{\n"
            for k, v in pairs(tbl) do
               local str_indent = 1
               acc[#acc + 1] = (" "):rep((indent + 1) * multiplier)
               key_to_str(k, acc, indent + 1)
               if acc[#acc] == " ]" and acc[#acc - 2] == "[ " then
                  str_indent = 8 + #acc[#acc - 1]
               end
               acc[#acc + 1] = " = "
               val_to_str_(v, acc, indent + 1, str_indent)
               acc[#acc + 1] = "\n"
            end
            acc[#acc + 1] = (" "):rep(indent * multiplier)
            acc[#acc + 1] = "}"
         end
         function util.expose(v)
            print(val_to_string(v))
            return v
         end
         function util.map(ary, func, ...)
            if type(ary) == "function" then
               ary, func = func, ary
            end
            local res = {}
            for i = 1, #ary do
               res[i] = func(ary[i], ...)
            end
            return res
         end
         function util.selfmap(ary, func, ...)
            if type(ary) == "function" then
               ary, func = func, ary
            end
            for i = 1, #ary do
               ary[i] = func(ary[i], ...)
            end
            return ary
         end
         local function map_all(tbl, func, ...)
            if type(tbl) == "function" then
               tbl, func = func, tbl
            end
            local res = {}
            for k, v in next, tbl do
               res[k] = func(v, ...)
            end
            return res
         end
         util.map_all = map_all
         local function fold(ary, func, acc)
            local i0 = 1
            if not acc then
               acc = ary[1]
               i0 = 2
            end
            for i = i0, #ary do
               acc = func(acc, ary[i])
            end
            return acc
         end
         util.fold = fold
         local function foldr(ary, func, acc)
            local offset = 0
            if not acc then
               acc = ary[#ary]
               offset = 1
            end
            for i = #ary - offset, 1, -1 do
               acc = func(ary[i], acc)
            end
            return acc
         end
         util.foldr = foldr
         local function map_fold(ary, mfunc, ffunc, acc)
            local i0 = 1
            if not acc then
               acc = mfunc(ary[1])
               i0 = 2
            end
            for i = i0, #ary do
               acc = ffunc(acc, mfunc(ary[i]))
            end
            return acc
         end
         util.map_fold = map_fold
         local function map_foldr(ary, mfunc, ffunc, acc)
            local offset = 0
            if not acc then
               acc = mfunc(ary[#acc])
               offset = 1
            end
            for i = #ary - offset, 1, -1 do
               acc = ffunc(mfunc(ary[i], acc))
            end
            return acc
         end
         util.map_foldr = map_fold
         function util.zip(a1, a2)
            local res, len = {}, m_max(#a1, #a2)
            for i = 1, len do
               res[i] = { a1[i], a2[i] }
            end
            return res
         end
         function util.zip_all(t1, t2)
            local res = {}
            for k, v in pairs(t1) do
               res[k] = { v, t2[k] }
            end
            for k, v in pairs(t2) do
               if res[k] == nil then
                  res[k] = { t1[k], v }
               end
            end
            return res
         end
         function util.filter(ary, func)
            local res = {}
            for i = 1, #ary do
               if func(ary[i]) then
                  t_insert(res, ary[i])
               end
            end
         end
         local function id(...)
            return ...
         end
         util.id = id
         local function AND(a, b)
            return a and b
         end
         local function OR(a, b)
            return a or b
         end
         function util.copy(tbl)
            return map_all(tbl, id)
         end
         function util.all(ary, mfunc)
            if mfunc then
               return map_fold(ary, mfunc, AND)
            else
               return fold(ary, AND)
            end
         end
         function util.any(ary, mfunc)
            if mfunc then
               return map_fold(ary, mfunc, OR)
            else
               return fold(ary, OR)
            end
         end
         function util.get(field)
            return function(tbl)
               return tbl[field]
            end
         end
         function util.lt(ref)
            return function(val)
               return val < ref
            end
         end
         function util.compose(f, g)
            return function(...)
               return f(g(...))
            end
         end
         function util.extend(destination, ...)
            for i = 1, select("#", ...) do
               for k, v in pairs((select(i, ...))) do
                  destination[k] = v
               end
            end
            return destination
         end
         function util.setify(t)
            local set = {}
            for i = 1, #t do
               set[t[i]] = true
            end
            return set
         end
         function util.arrayify(...)
            return { ... }
         end
         local function _checkstrhelper(s)
            return s .. ""
         end
         function util.checkstring(s, func)
            local success, str = pcall(_checkstrhelper, s)
            if not success then
               if func == nil then
                  func = "?"
               end
               error("bad argument to '" .. tostring(func) .. "' (string expected, got " .. type(s) .. ")", 2)
            end
            return str
         end
         return util
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["re"] = function(...)
         return function(Builder, LL)
            local tonumber, type, print, error = tonumber, type, print, error
            local setmetatable = setmetatable
            local m = LL
            local mm = m
            local mt = getmetatable(mm.P(0))
            local version = _VERSION
            if version == "Lua 5.2" then
               _ENV = nil
            end
            local any = m.P(1)
            local Predef = { nl = m.P "\n" }
            local mem
            local fmem
            local gmem
            local function updatelocale()
               mm.locale(Predef)
               Predef.a = Predef.alpha
               Predef.c = Predef.cntrl
               Predef.d = Predef.digit
               Predef.g = Predef.graph
               Predef.l = Predef.lower
               Predef.p = Predef.punct
               Predef.s = Predef.space
               Predef.u = Predef.upper
               Predef.w = Predef.alnum
               Predef.x = Predef.xdigit
               Predef.A = any - Predef.a
               Predef.C = any - Predef.c
               Predef.D = any - Predef.d
               Predef.G = any - Predef.g
               Predef.L = any - Predef.l
               Predef.P = any - Predef.p
               Predef.S = any - Predef.s
               Predef.U = any - Predef.u
               Predef.W = any - Predef.w
               Predef.X = any - Predef.x
               mem = {} -- restart memoization
               fmem = {}
               gmem = {}
               local mt = { __mode = "v" }
               setmetatable(mem, mt)
               setmetatable(fmem, mt)
               setmetatable(gmem, mt)
            end
            updatelocale()
            local function getdef(id, defs)
               local c = defs and defs[id]
               if not c then
                  error("undefined name: " .. id)
               end
               return c
            end
            local function patt_error(s, i)
               local msg = (#s < i + 20) and s:sub(i) or s:sub(i, i + 20) .. "..."
               msg = ("pattern error near '%s'"):format(msg)
               error(msg, 2)
            end
            local function mult(p, n)
               local np = mm.P(true)
               while n >= 1 do
                  if n % 2 >= 1 then
                     np = np * p
                  end
                  p = p * p
                  n = n / 2
               end
               return np
            end
            local function equalcap(s, i, c)
               if type(c) ~= "string" then
                  return nil
               end
               local e = #c + i
               if s:sub(i, e - 1) == c then
                  return e
               else
                  return nil
               end
            end
            local S = (Predef.space + "--" * (any - Predef.nl) ^ 0) ^ 0
            local name = m.R("AZ", "az", "__") * m.R("AZ", "az", "__", "09") ^ 0
            local arrow = S * "<-"
            local seq_follow = m.P "/" + ")" + "}" + ":}" + "~}" + "|}" + (name * arrow) + -1
            name = m.C(name)
            local Def = name * m.Carg(1)
            local num = m.C(m.R "09" ^ 1) * S / tonumber
            local String = "'" * m.C((any - "'") ^ 0) * "'" + '"' * m.C((any - '"') ^ 0) * '"'
            local defined = "%"
               * Def
               / function(c, Defs)
                  local cat = Defs and Defs[c] or Predef[c]
                  if not cat then
                     error("name '" .. c .. "' undefined")
                  end
                  return cat
               end
            local Range = m.Cs(any * (m.P "-" / "") * (any - "]")) / mm.R
            local item = defined + Range + m.C(any)
            local Class = "["
               * (m.C(m.P "^" ^ -1)) -- optional complement symbol
               * m.Cf(item * (item - "]") ^ 0, mt.__add)
               / function(c, p)
                  return c == "^" and any - p or p
               end
               * "]"
            local function adddef(t, k, exp)
               if t[k] then
                  error("'" .. k .. "' already defined as a rule")
               else
                  t[k] = exp
               end
               return t
            end
            local function firstdef(n, r)
               return adddef({ n }, n, r)
            end
            local function NT(n, b)
               if not b then
                  error("rule '" .. n .. "' used outside a grammar")
               else
                  return mm.V(n)
               end
            end
            local exp = m.P {
               "Exp",
               Exp = S * (m.V "Grammar" + m.Cf(m.V "Seq" * ("/" * S * m.V "Seq") ^ 0, mt.__add)),
               Seq = m.Cf(m.Cc(m.P "") * m.V "Prefix" ^ 0, mt.__mul) * (m.L(seq_follow) + patt_error),
               Prefix = "&" * S * m.V "Prefix" / mt.__len + "!" * S * m.V "Prefix" / mt.__unm + m.V "Suffix",
               Suffix = m.Cf(
                  m.V "Primary"
                     * S
                     * ((m.P "+" * m.Cc(1, mt.__pow) + m.P "*" * m.Cc(0, mt.__pow) + m.P "?" * m.Cc(-1, mt.__pow) + "^" * (m.Cg(
                           num * m.Cc(mult)
                        ) + m.Cg(m.C(m.S "+-" * m.R "09" ^ 1) * m.Cc(mt.__pow))) + "->" * S * (m.Cg(
                           (String + num) * m.Cc(mt.__div)
                        ) + m.P "{}" * m.Cc(nil, m.Ct) + m.Cg(Def / getdef * m.Cc(mt.__div))) + "=>" * S * m.Cg(
                           Def / getdef * m.Cc(m.Cmt)
                        )) * S)
                        ^ 0,
                  function(a, b, f)
                     return f(a, b)
                  end
               ),
               Primary = "(" * m.V "Exp" * ")"
                  + String / mm.P
                  + Class
                  + defined
                  + "{:" * (name * ":" + m.Cc(nil)) * m.V "Exp" * ":}" / function(n, p)
                     return mm.Cg(p, n)
                  end
                  + "=" * name / function(n)
                     return mm.Cmt(mm.Cb(n), equalcap)
                  end
                  + m.P "{}" / mm.Cp
                  + "{~" * m.V "Exp" * "~}" / mm.Cs
                  + "{|" * m.V "Exp" * "|}" / mm.Ct
                  + "{" * m.V "Exp" * "}" / mm.C
                  + m.P "." * m.Cc(any)
                  + (name * -arrow + "<" * name * ">") * m.Cb "G" / NT,
               Definition = name * arrow * m.V "Exp",
               Grammar = m.Cg(m.Cc(true), "G")
                  * m.Cf(m.V "Definition" / firstdef * m.Cg(m.V "Definition") ^ 0, adddef)
                  / mm.P,
            }
            local pattern = S * m.Cg(m.Cc(false), "G") * exp / mm.P * (-any + patt_error)
            local function compile(p, defs)
               if mm.type(p) == "pattern" then
                  return p
               end -- already compiled
               local cp = pattern:match(p, 1, defs)
               if not cp then
                  error("incorrect pattern", 3)
               end
               return cp
            end
            local function match(s, p, i)
               local cp = mem[p]
               if not cp then
                  cp = compile(p)
                  mem[p] = cp
               end
               return cp:match(s, i or 1)
            end
            local function find(s, p, i)
               local cp = fmem[p]
               if not cp then
                  cp = compile(p) / 0
                  cp = mm.P { mm.Cp() * cp * mm.Cp() + 1 * mm.V(1) }
                  fmem[p] = cp
               end
               local i, e = cp:match(s, i or 1)
               if i then
                  return i, e - 1
               else
                  return i
               end
            end
            local function gsub(s, p, rep)
               local g = gmem[p] or {} -- ensure gmem[p] is not collected while here
               gmem[p] = g
               local cp = g[rep]
               if not cp then
                  cp = compile(p)
                  cp = mm.Cs((cp / rep + 1) ^ 0)
                  g[rep] = cp
               end
               return cp:match(s)
            end
            local re = {
               compile = compile,
               match = match,
               find = find,
               gsub = gsub,
               updatelocale = updatelocale,
            }
            return re
         end
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["evaluator"] = function(...)
         local select, tonumber, tostring, type = select, tonumber, tostring, type
         local s, t, u = require "string", require "table", require "util"
         local s_sub, t_concat = s.sub, t.concat
         local t_unpack = u.unpack
         local _ENV = u.noglobals() ----------------------------------------------------
         return function(Builder, LL) -- Decorator wrapper
            local eval = {}
            local function insert(caps, sbj, vals, ci, vi)
               local openclose, kind = caps.openclose, caps.kind
               while kind[ci] and openclose[ci] >= 0 do
                  ci, vi = eval[kind[ci]](caps, sbj, vals, ci, vi)
               end
               return ci, vi
            end
            function eval.C(caps, sbj, vals, ci, vi)
               if caps.openclose[ci] > 0 then
                  vals[vi] = s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
                  return ci + 1, vi + 1
               end
               vals[vi] = false -- pad it for now
               local cj, vj = insert(caps, sbj, vals, ci + 1, vi + 1)
               vals[vi] = s_sub(sbj, caps.bounds[ci], caps.bounds[cj] - 1)
               return cj + 1, vj
            end
            local function lookback(caps, label, ci)
               local aux, openclose, kind = caps.aux, caps.openclose, caps.kind
               repeat
                  ci = ci - 1
                  local auxv, oc = aux[ci], openclose[ci]
                  if oc < 0 then
                     ci = ci + oc
                  end
                  if oc ~= 0 and kind[ci] == "Clb" and label == auxv then
                     return ci
                  end
               until ci == 1
               label = type(label) == "string" and "'" .. label .. "'" or tostring(label)
               error("back reference " .. label .. " not found")
            end
            function eval.Cb(caps, sbj, vals, ci, vi)
               local Cb_ci = lookback(caps, caps.aux[ci], ci)
               Cb_ci, vi = eval.Cg(caps, sbj, vals, Cb_ci, vi)
               return ci + 1, vi
            end
            function eval.Cc(caps, sbj, vals, ci, vi)
               local these_values = caps.aux[ci]
               for i = 1, these_values.n do
                  vi, vals[vi] = vi + 1, these_values[i]
               end
               return ci + 1, vi
            end
            eval["Cf"] = function()
               error "NYI: Cf"
            end
            function eval.Cf(caps, sbj, vals, ci, vi)
               if caps.openclose[ci] > 0 then
                  error "No First Value"
               end
               local func, Cf_vals, Cf_vi = caps.aux[ci], {}
               ci = ci + 1
               ci, Cf_vi = eval[caps.kind[ci]](caps, sbj, Cf_vals, ci, 1)
               if Cf_vi == 1 then
                  error "No first value"
               end
               local result = Cf_vals[1]
               while caps.kind[ci] and caps.openclose[ci] >= 0 do
                  ci, Cf_vi = eval[caps.kind[ci]](caps, sbj, Cf_vals, ci, 1)
                  result = func(result, t_unpack(Cf_vals, 1, Cf_vi - 1))
               end
               vals[vi] = result
               return ci + 1, vi + 1
            end
            function eval.Cg(caps, sbj, vals, ci, vi)
               if caps.openclose[ci] > 0 then
                  vals[vi] = s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
                  return ci + 1, vi + 1
               end
               local cj, vj = insert(caps, sbj, vals, ci + 1, vi)
               if vj == vi then
                  vals[vj] = s_sub(sbj, caps.bounds[ci], caps.bounds[cj] - 1)
                  vj = vj + 1
               end
               return cj + 1, vj
            end
            function eval.Clb(caps, sbj, vals, ci, vi)
               local oc = caps.openclose
               if oc[ci] > 0 then
                  return ci + 1, vi
               end
               local depth = 0
               repeat
                  if oc[ci] == 0 then
                     depth = depth + 1
                  elseif oc[ci] < 0 then
                     depth = depth - 1
                  end
                  ci = ci + 1
               until depth == 0
               return ci, vi
            end
            function eval.Cp(caps, sbj, vals, ci, vi)
               vals[vi] = caps.bounds[ci]
               return ci + 1, vi + 1
            end
            function eval.Ct(caps, sbj, vals, ci, vi)
               local aux, openclose, kind = caps.aux, caps.openclose, caps.kind
               local tbl_vals = {}
               vals[vi] = tbl_vals
               if openclose[ci] > 0 then
                  return ci + 1, vi + 1
               end
               local tbl_vi, Clb_vals = 1, {}
               ci = ci + 1
               while kind[ci] and openclose[ci] >= 0 do
                  if kind[ci] == "Clb" then
                     local label, Clb_vi = aux[ci], 1
                     ci, Clb_vi = eval.Cg(caps, sbj, Clb_vals, ci, 1)
                     if Clb_vi ~= 1 then
                        tbl_vals[label] = Clb_vals[1]
                     end
                  else
                     ci, tbl_vi = eval[kind[ci]](caps, sbj, tbl_vals, ci, tbl_vi)
                  end
               end
               return ci + 1, vi + 1
            end
            local inf = 1 / 0
            function eval.value(caps, sbj, vals, ci, vi)
               local val
               if caps.aux[ci] ~= inf or caps.openclose[ci] ~= inf then
                  val = caps.aux[ci]
               end
               vals[vi] = val
               return ci + 1, vi + 1
            end
            function eval.Cs(caps, sbj, vals, ci, vi)
               if caps.openclose[ci] > 0 then
                  vals[vi] = s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
               else
                  local bounds, kind, openclose = caps.bounds, caps.kind, caps.openclose
                  local start, buffer, Cs_vals, bi, Cs_vi = bounds[ci], {}, {}, 1, 1
                  local last
                  ci = ci + 1
                  while openclose[ci] >= 0 do
                     last = bounds[ci]
                     buffer[bi] = s_sub(sbj, start, last - 1)
                     bi = bi + 1
                     ci, Cs_vi = eval[kind[ci]](caps, sbj, Cs_vals, ci, 1)
                     if Cs_vi > 1 then
                        buffer[bi] = Cs_vals[1]
                        bi = bi + 1
                        start = openclose[ci - 1] > 0 and openclose[ci - 1] or bounds[ci - 1]
                     else
                        start = last
                     end
                  end
                  buffer[bi] = s_sub(sbj, start, bounds[ci] - 1)
                  vals[vi] = t_concat(buffer)
               end
               return ci + 1, vi + 1
            end
            local function insert_divfunc_results(acc, val_i, ...)
               local n = select("#", ...)
               for i = 1, n do
                  val_i, acc[val_i] = val_i + 1, select(i, ...)
               end
               return val_i
            end
            function eval.div_function(caps, sbj, vals, ci, vi)
               local func = caps.aux[ci]
               local params, divF_vi
               if caps.openclose[ci] > 0 then
                  params, divF_vi = { s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1) }, 2
               else
                  params = {}
                  ci, divF_vi = insert(caps, sbj, params, ci + 1, 1)
               end
               ci = ci + 1 -- skip the closed or closing node.
               vi = insert_divfunc_results(vals, vi, func(t_unpack(params, 1, divF_vi - 1)))
               return ci, vi
            end
            function eval.div_number(caps, sbj, vals, ci, vi)
               local this_aux = caps.aux[ci]
               local divN_vals, divN_vi
               if caps.openclose[ci] > 0 then
                  divN_vals, divN_vi = { s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1) }, 2
               else
                  divN_vals = {}
                  ci, divN_vi = insert(caps, sbj, divN_vals, ci + 1, 1)
               end
               ci = ci + 1 -- skip the closed or closing node.
               if this_aux >= divN_vi then
                  error("no capture '" .. this_aux .. "' in /number capture.")
               end
               vals[vi] = divN_vals[this_aux]
               return ci, vi + 1
            end
            local function div_str_cap_refs(caps, ci)
               local opcl = caps.openclose
               local refs = { open = caps.bounds[ci] }
               if opcl[ci] > 0 then
                  refs.close = opcl[ci]
                  return ci + 1, refs, 0
               end
               local first_ci = ci
               local depth = 1
               ci = ci + 1
               repeat
                  local oc = opcl[ci]
                  if depth == 1 and oc >= 0 then
                     refs[#refs + 1] = ci
                  end
                  if oc == 0 then
                     depth = depth + 1
                  elseif oc < 0 then
                     depth = depth - 1
                  end
                  ci = ci + 1
               until depth == 0
               refs.close = caps.bounds[ci - 1]
               return ci, refs, #refs
            end
            function eval.div_string(caps, sbj, vals, ci, vi)
               local n, refs
               local cached
               local cached, divS_vals = {}, {}
               local the_string = caps.aux[ci]
               ci, refs, n = div_str_cap_refs(caps, ci)
               vals[vi] = the_string:gsub("%%([%d%%])", function(d)
                  if d == "%" then
                     return "%"
                  end
                  d = tonumber(d)
                  if not cached[d] then
                     if d > n then
                        error("no capture at index " .. d .. " in /string capture.")
                     end
                     if d == 0 then
                        cached[d] = s_sub(sbj, refs.open, refs.close - 1)
                     else
                        local _, vi = eval[caps.kind[refs[d]]](caps, sbj, divS_vals, refs[d], 1)
                        if vi == 1 then
                           error("no values in capture at index" .. d .. " in /string capture.")
                        end
                        cached[d] = divS_vals[1]
                     end
                  end
                  return cached[d]
               end)
               return ci, vi + 1
            end
            function eval.div_table(caps, sbj, vals, ci, vi)
               local this_aux = caps.aux[ci]
               local key
               if caps.openclose[ci] > 0 then
                  key = s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1)
               else
                  local divT_vals, _ = {}
                  ci, _ = insert(caps, sbj, divT_vals, ci + 1, 1)
                  key = divT_vals[1]
               end
               ci = ci + 1
               if this_aux[key] then
                  vals[vi] = this_aux[key]
                  return ci, vi + 1
               else
                  return ci, vi
               end
            end
            function LL.evaluate(caps, sbj, ci)
               local vals = {}
               local _, vi = insert(caps, sbj, vals, ci, 1)
               return vals, 1, vi - 1
            end
         end -- Decorator wrapper
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["printers"] = function(...)
         return function(Builder, LL)
            local ipairs, pairs, print, tostring, type = ipairs, pairs, print, tostring, type
            local s, t, u = require "string", require "table", require "util"
            local S_tostring = Builder.set.tostring
            local _ENV = u.noglobals() ----------------------------------------------------
            local s_char, s_sub, t_concat = s.char, s.sub, t.concat
            local expose, load, map = u.expose, u.load, u.map
            local escape_index = {
               ["\f"] = "\\f",
               ["\n"] = "\\n",
               ["\r"] = "\\r",
               ["\t"] = "\\t",
               ["\v"] = "\\v",
               ["\127"] = "\\ESC",
            }
            local function flatten(kind, list)
               if list[2].pkind == kind then
                  return list[1], flatten(kind, list[2])
               else
                  return list[1], list[2]
               end
            end
            for i = 0, 8 do
               escape_index[s_char(i)] = "\\" .. i
            end
            for i = 14, 31 do
               escape_index[s_char(i)] = "\\" .. i
            end
            local function escape(str)
               return str:gsub("%c", escape_index)
            end
            local function set_repr(set)
               return s_char(load("return " .. S_tostring(set))())
            end
            local printers = {}
            local function LL_pprint(pt, offset, prefix)
               return printers[pt.pkind](pt, offset, prefix)
            end
            function LL.pprint(pt0)
               local pt = LL.P(pt0)
               print "\nPrint pattern"
               LL_pprint(pt, "", "")
               print "--- /pprint\n"
               return pt0
            end
            for k, v in pairs {
               string = [[ "P( \""..escape(pt.as_is).."\" )"       ]],
               char = [[ "P( \""..escape(to_char(pt.aux)).."\" )"]],
               ["true"] = [[ "P( true )"                     ]],
               ["false"] = [[ "P( false )"                    ]],
               eos = [[ "~EOS~"                         ]],
               one = [[ "P( one )"                      ]],
               any = [[ "P( "..pt.aux.." )"             ]],
               set = [[ "S( "..'"'..escape(set_repr(pt.aux))..'"'.." )" ]],
               ["function"] = [[ "P( "..pt.aux.." )"             ]],
               ref = [[
        "V( ",
            (type(pt.aux) == "string" and "\""..pt.aux.."\"")
                          or tostring(pt.aux)
        , " )"
        ]],
               range = [[
        "R( ",
            escape(t_concat(map(
                pt.as_is,
                function(e) return '"'..e..'"' end)
            , ", "))
        ," )"
        ]],
            } do
               printers[k] = load(
                  ([==[
        local k, map, t_concat, to_char, escape, set_repr = ...
        return function (pt, offset, prefix)
            print(t_concat{offset,prefix,XXXX})
        end
    ]==]):gsub("XXXX", v),
                  k .. " printer"
               )(k, map, t_concat, s_char, escape, set_repr)
            end
            for k, v in pairs {
               ["behind"] = [[ LL_pprint(pt.pattern, offset, "B ") ]],
               ["at least"] = [[ LL_pprint(pt.pattern, offset, pt.aux.." ^ ") ]],
               ["at most"] = [[ LL_pprint(pt.pattern, offset, pt.aux.." ^ ") ]],
               unm = [[LL_pprint(pt.pattern, offset, "- ")]],
               lookahead = [[LL_pprint(pt.pattern, offset, "# ")]],
               choice = [[
        print(offset..prefix.."+")
        local ch, i = {}, 1
        while pt.pkind == "choice" do
            ch[i], pt, i = pt[1], pt[2], i + 1
        end
        ch[i] = pt
        map(ch, LL_pprint, offset.." :", "")
        ]],
               sequence = [=[
        print(offset..prefix.."*")
        local acc, p2 = {}
        offset = offset .. " |"
        while true do
            if pt.pkind ~= "sequence" then -- last element
                if pt.pkind == "char" then
                    acc[#acc + 1] = pt.aux
                    print(offset..'P( "'..s.char(u.unpack(acc))..'" )')
                else
                    if #acc ~= 0 then
                        print(offset..'P( "'..s.char(u.unpack(acc))..'" )')
                    end
                    LL_pprint(pt, offset, "")
                end
                break
            elseif pt[1].pkind == "char" then
                acc[#acc + 1] = pt[1].aux
            elseif #acc ~= 0 then
                print(offset..'P( "'..s.char(u.unpack(acc))..'" )')
                acc = {}
                LL_pprint(pt[1], offset, "")
            else
                LL_pprint(pt[1], offset, "")
            end
            pt = pt[2]
        end
        ]=],
               grammar = [[
        print(offset..prefix.."Grammar")
        for k, pt in pairs(pt.aux) do
            local prefix = ( type(k)~="string"
                             and tostring(k)
                             or "\""..k.."\"" )
            LL_pprint(pt, offset.."  ", prefix .. " = ")
        end
    ]],
            } do
               printers[k] = load(
                  ([[
        local map, LL_pprint, pkind, s, u, flatten = ...
        return function (pt, offset, prefix)
            XXXX
        end
    ]]):gsub("XXXX", v),
                  k .. " printer"
               )(map, LL_pprint, type, s, u, flatten)
            end
            for _, cap in pairs { "C", "Cs", "Ct" } do
               printers[cap] = function(pt, offset, prefix)
                  print(offset .. prefix .. cap)
                  LL_pprint(pt.pattern, offset .. "  ", "")
               end
            end
            for _, cap in pairs { "Cg", "Clb", "Cf", "Cmt", "div_number", "/zero", "div_function", "div_table" } do
               printers[cap] = function(pt, offset, prefix)
                  print(offset .. prefix .. cap .. " " .. tostring(pt.aux or ""))
                  LL_pprint(pt.pattern, offset .. "  ", "")
               end
            end
            printers["div_string"] = function(pt, offset, prefix)
               print(offset .. prefix .. '/string "' .. tostring(pt.aux or "") .. '"')
               LL_pprint(pt.pattern, offset .. "  ", "")
            end
            for _, cap in pairs { "Carg", "Cp" } do
               printers[cap] = function(pt, offset, prefix)
                  print(offset .. prefix .. cap .. "( " .. tostring(pt.aux) .. " )")
               end
            end
            printers["Cb"] = function(pt, offset, prefix)
               print(offset .. prefix .. 'Cb( "' .. pt.aux .. '" )')
            end
            printers["Cc"] = function(pt, offset, prefix)
               print(offset .. prefix .. "Cc(" .. t_concat(map(pt.aux, tostring), ", ") .. " )")
            end
            local cprinters = {}
            local padding = "   "
            local function padnum(n)
               n = tostring(n)
               n = n .. "." .. ((" "):rep(4 - #n))
               return n
            end
            local function _cprint(caps, ci, indent, sbj, n)
               local openclose, kind = caps.openclose, caps.kind
               indent = indent or 0
               while kind[ci] and openclose[ci] >= 0 do
                  if caps.openclose[ci] > 0 then
                     print(t_concat {
                        padnum(n),
                        padding:rep(indent),
                        caps.kind[ci],
                        ": start = ",
                        tostring(caps.bounds[ci]),
                        " finish = ",
                        tostring(caps.openclose[ci]),
                        caps.aux[ci] and " aux = " or "",
                        caps.aux[ci]
                              and (type(caps.aux[ci]) == "string" and '"' .. tostring(caps.aux[ci]) .. '"' or tostring(
                                 caps.aux[ci]
                              ))
                           or "",
                        " \t",
                        s_sub(sbj, caps.bounds[ci], caps.openclose[ci] - 1),
                     })
                     if type(caps.aux[ci]) == "table" then
                        expose(caps.aux[ci])
                     end
                  else
                     local kind = caps.kind[ci]
                     local start = caps.bounds[ci]
                     print(t_concat {
                        padnum(n),
                        padding:rep(indent),
                        kind,
                        ": start = ",
                        start,
                        caps.aux[ci] and " aux = " or "",
                        caps.aux[ci]
                              and (type(caps.aux[ci]) == "string" and '"' .. tostring(caps.aux[ci]) .. '"' or tostring(
                                 caps.aux[ci]
                              ))
                           or "",
                     })
                     ci, n = _cprint(caps, ci + 1, indent + 1, sbj, n + 1)
                     print(t_concat {
                        padnum(n),
                        padding:rep(indent),
                        "/",
                        kind,
                        " finish = ",
                        tostring(caps.bounds[ci]),
                        " \t",
                        s_sub(sbj, start, (caps.bounds[ci] or 1) - 1),
                     })
                  end
                  n = n + 1
                  ci = ci + 1
               end
               return ci, n
            end
            function LL.cprint(caps, ci, sbj)
               ci = ci or 1
               print "\nCapture Printer:\n================"
               _cprint(caps, ci, 0, sbj, 1)
               print "================\n/Cprinter\n"
            end
            return { pprint = LL.pprint, cprint = LL.cprint }
         end -- module wrapper ---------------------------------------------------------
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["compat"] = function(...)
         local _, debug, jit
         _, debug = pcall(require, "debug")
         _, jit = pcall(require, "jit")
         jit = _ and jit
         local compat = {
            debug = debug,
            lua51 = (_VERSION == "Lua 5.1") and not jit,
            lua52 = _VERSION == "Lua 5.2",
            luajit = jit and true or false,
            jit = jit and jit.status(),
            lua52_len = not #setmetatable({}, { __len = function() end }),
            proxies = pcall(function()
               local prox = newproxy(true)
               local prox2 = newproxy(prox)
               assert(type(getmetatable(prox)) == "table" and (getmetatable(prox)) == (getmetatable(prox2)))
            end),
            _goto = not not (loadstring or load) "::R::",
         }
         return compat
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["locale"] = function(...)
         local extend = require("util").extend
         local _ENV = require("util").noglobals() ----------------------------------------
         return function(Builder, LL) -- Module wrapper {-------------------------------
            local R, S = LL.R, LL.S
            local locale = {}
            locale["cntrl"] = R "\0\31" + "\127"
            locale["digit"] = R "09"
            locale["lower"] = R "az"
            locale["print"] = R " ~" -- 0x20 to 0xee
            locale["space"] = S " \f\n\r\t\v" -- \f == form feed (for a printer), \v == vtab
            locale["upper"] = R "AZ"
            locale["alpha"] = locale["lower"] + locale["upper"]
            locale["alnum"] = locale["alpha"] + locale["digit"]
            locale["graph"] = locale["print"] - locale["space"]
            locale["punct"] = locale["graph"] - locale["alnum"]
            locale["xdigit"] = locale["digit"] + R "af" + R "AF"
            function LL.locale(t)
               return extend(t or {}, locale)
            end
         end -- Module wrapper --------------------------------------------------------}
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["match"] = function(...) end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["charsets"] = function(...)
         local s, t, u = require "string", require "table", require "util"
         local _ENV = u.noglobals() ----------------------------------------------------
         local copy = u.copy
         local s_char, s_sub, s_byte, t_concat, t_insert = s.char, s.sub, s.byte, t.concat, t.insert
         local function utf8_offset(byte)
            if byte < 128 then
               return 0, byte
            elseif byte < 192 then
               error "Byte values between 0x80 to 0xBF cannot start a multibyte sequence"
            elseif byte < 224 then
               return 1, byte - 192
            elseif byte < 240 then
               return 2, byte - 224
            elseif byte < 248 then
               return 3, byte - 240
            elseif byte < 252 then
               return 4, byte - 248
            elseif byte < 254 then
               return 5, byte - 252
            else
               error "Byte values between 0xFE and OxFF cannot start a multibyte sequence"
            end
         end
         local function utf8_validate(subject, start, finish)
            start = start or 1
            finish = finish or #subject
            local offset, char = 0
            for i = start, finish do
               local b = s_byte(subject, i)
               if offset == 0 then
                  char = i
                  success, offset = pcall(utf8_offset, b)
                  if not success then
                     return false, char - 1
                  end
               else
                  if not (127 < b and b < 192) then
                     return false, char - 1
                  end
                  offset = offset - 1
               end
            end
            if offset ~= 0 then
               return nil, char - 1
            end -- Incomplete input.
            return true, finish
         end
         local function utf8_next_int(subject, i)
            i = i and i + 1 or 1
            if i > #subject then
               return
            end
            local c = s_byte(subject, i)
            local offset, val = utf8_offset(c)
            for i = i + 1, i + offset do
               c = s_byte(subject, i)
               val = val * 64 + (c - 128)
            end
            return i + offset, i, val
         end
         local function utf8_next_char(subject, i)
            i = i and i + 1 or 1
            if i > #subject then
               return
            end
            local offset = utf8_offset(s_byte(subject, i))
            return i + offset, i, s_sub(subject, i, i + offset)
         end
         local function utf8_split_int(subject)
            local chars = {}
            for _, _, c in utf8_next_int, subject do
               t_insert(chars, c)
            end
            return chars
         end
         local function utf8_split_char(subject)
            local chars = {}
            for _, _, c in utf8_next_char, subject do
               t_insert(chars, c)
            end
            return chars
         end
         local function utf8_get_int(subject, i)
            if i > #subject then
               return
            end
            local c = s_byte(subject, i)
            local offset, val = utf8_offset(c)
            for i = i + 1, i + offset do
               c = s_byte(subject, i)
               val = val * 64 + (c - 128)
            end
            return val, i + offset + 1
         end
         local function split_generator(get)
            if not get then
               return
            end
            return function(subject)
               local res = {}
               local o, i = true
               while o do
                  o, i = get(subject, i)
                  res[#res] = o
               end
               return res
            end
         end
         local function merge_generator(char)
            if not char then
               return
            end
            return function(ary)
               local res = {}
               for i = 1, #ary do
                  t_insert(res, char(ary[i]))
               end
               return t_concat(res)
            end
         end
         local function utf8_get_int2(subject, i)
            local byte, b5, b4, b3, b2, b1 = s_byte(subject, i)
            if byte < 128 then
               return byte, i + 1
            elseif byte < 192 then
               error "Byte values between 0x80 to 0xBF cannot start a multibyte sequence"
            elseif byte < 224 then
               return (byte - 192) * 64 + s_byte(subject, i + 1), i + 2
            elseif byte < 240 then
               b2, b1 = s_byte(subject, i + 1, i + 2)
               return (byte - 224) * 4096 + b2 % 64 * 64 + b1 % 64, i + 3
            elseif byte < 248 then
               b3, b2, b1 = s_byte(subject, i + 1, i + 2, 1 + 3)
               return (byte - 240) * 262144 + b3 % 64 * 4096 + b2 % 64 * 64 + b1 % 64, i + 4
            elseif byte < 252 then
               b4, b3, b2, b1 = s_byte(subject, i + 1, i + 2, 1 + 3, i + 4)
               return (byte - 248) * 16777216 + b4 % 64 * 262144 + b3 % 64 * 4096 + b2 % 64 * 64 + b1 % 64, i + 5
            elseif byte < 254 then
               b5, b4, b3, b2, b1 = s_byte(subject, i + 1, i + 2, 1 + 3, i + 4, i + 5)
               return (byte - 252) * 1073741824
                  + b5 % 64 * 16777216
                  + b4 % 64 * 262144
                  + b3 % 64 * 4096
                  + b2 % 64 * 64
                  + b1 % 64,
                  i + 6
            else
               error "Byte values between 0xFE and OxFF cannot start a multibyte sequence"
            end
         end
         local function utf8_get_char(subject, i)
            if i > #subject then
               return
            end
            local offset = utf8_offset(s_byte(subject, i))
            return s_sub(subject, i, i + offset), i + offset + 1
         end
         local function utf8_char(c)
            if c < 128 then
               return s_char(c)
            elseif c < 2048 then
               return s_char(192 + c / 64, 128 + c % 64)
            elseif c < 55296 or 57343 < c and c < 65536 then
               return s_char(224 + c / 4096, 128 + c / 64 % 64, 128 + c % 64)
            elseif c < 2097152 then
               return s_char(240 + c / 262144, 128 + c / 4096 % 64, 128 + c / 64 % 64, 128 + c % 64)
            elseif c < 67108864 then
               return s_char(
                  248 + c / 16777216,
                  128 + c / 262144 % 64,
                  128 + c / 4096 % 64,
                  128 + c / 64 % 64,
                  128 + c % 64
               )
            elseif c < 2147483648 then
               return s_char(
                  252 + c / 1073741824,
                  128 + c / 16777216 % 64,
                  128 + c / 262144 % 64,
                  128 + c / 4096 % 64,
                  128 + c / 64 % 64,
                  128 + c % 64
               )
            end
            error("Bad Unicode code point: " .. c .. ".")
         end
         local function binary_validate(subject, start, finish)
            start = start or 1
            finish = finish or #subject
            return true, finish
         end
         local function binary_next_int(subject, i)
            i = i and i + 1 or 1
            if i >= #subject then
               return
            end
            return i, i, s_sub(subject, i, i)
         end
         local function binary_next_char(subject, i)
            i = i and i + 1 or 1
            if i > #subject then
               return
            end
            return i, i, s_byte(subject, i)
         end
         local function binary_split_int(subject)
            local chars = {}
            for i = 1, #subject do
               t_insert(chars, s_byte(subject, i))
            end
            return chars
         end
         local function binary_split_char(subject)
            local chars = {}
            for i = 1, #subject do
               t_insert(chars, s_sub(subject, i, i))
            end
            return chars
         end
         local function binary_get_int(subject, i)
            return s_byte(subject, i), i + 1
         end
         local function binary_get_char(subject, i)
            return s_sub(subject, i, i), i + 1
         end
         local charsets = {
            binary = {
               name = "binary",
               binary = true,
               validate = binary_validate,
               split_char = binary_split_char,
               split_int = binary_split_int,
               next_char = binary_next_char,
               next_int = binary_next_int,
               get_char = binary_get_char,
               get_int = binary_get_int,
               tochar = s_char,
            },
            ["UTF-8"] = {
               name = "UTF-8",
               validate = utf8_validate,
               split_char = utf8_split_char,
               split_int = utf8_split_int,
               next_char = utf8_next_char,
               next_int = utf8_next_int,
               get_char = utf8_get_char,
               get_int = utf8_get_int,
            },
         }
         return function(Builder)
            local cs = Builder.options.charset or "binary"
            if charsets[cs] then
               Builder.charset = copy(charsets[cs])
               Builder.binary_split_int = binary_split_int
            else
               error "NYI: custom charsets"
            end
         end
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["API"] = function(...)
         local assert, error, ipairs, pairs, pcall, print, require, select, tonumber, tostring, type =
            assert, error, ipairs, pairs, pcall, print, require, select, tonumber, tostring, type
         local t, u = require "table", require "util"
         local _ENV = u.noglobals() ---------------------------------------------------
         local t_concat = t.concat
         local checkstring, copy, fold, load, map_fold, map_foldr, setify, t_pack, t_unpack =
            u.checkstring, u.copy, u.fold, u.load, u.map_fold, u.map_foldr, u.setify, u.pack, u.unpack
         local function charset_error(index, charset)
            error("Character at position " .. index + 1 .. " is not a valid " .. charset .. " one.", 2)
         end
         return function(Builder, LL) -- module wrapper -------------------------------
            local cs = Builder.charset
            local constructors, LL_ispattern = Builder.constructors, LL.ispattern
            local truept, falsept, Cppt =
               constructors.constant.truept, constructors.constant.falsept, constructors.constant.Cppt
            local split_int, validate = cs.split_int, cs.validate
            local Range, Set, S_union, S_tostring =
               Builder.Range, Builder.set.new, Builder.set.union, Builder.set.tostring
            local factorize_choice, factorize_lookahead, factorize_sequence, factorize_unm
            local function makechar(c)
               return constructors.aux("char", c)
            end
            local function LL_P(...)
               local v, n = (...), select("#", ...)
               if n == 0 then
                  error "bad argument #1 to 'P' (value expected)"
               end
               local typ = type(v)
               if LL_ispattern(v) then
                  return v
               elseif typ == "function" then
                  return LL.Cmt("", v)
               elseif typ == "string" then
                  local success, index = validate(v)
                  if not success then
                     charset_error(index, cs.name)
                  end
                  if v == "" then
                     return truept
                  end
                  return map_foldr(split_int(v), makechar, Builder.sequence)
               elseif typ == "table" then
                  local g = copy(v)
                  if g[1] == nil then
                     error "grammar has no initial rule"
                  end
                  if not LL_ispattern(g[1]) then
                     g[1] = LL.V(g[1])
                  end
                  return constructors.none("grammar", g)
               elseif typ == "boolean" then
                  return v and truept or falsept
               elseif typ == "number" then
                  if v == 0 then
                     return truept
                  elseif v > 0 then
                     return constructors.aux("any", v)
                  else
                     return -constructors.aux("any", -v)
                  end
               else
                  error("bad argument #1 to 'P' (lpeg-pattern expected, got " .. typ .. ")")
               end
            end
            LL.P = LL_P
            local function LL_S(set)
               if set == "" then
                  return falsept
               else
                  local success
                  set = checkstring(set, "S")
                  return constructors.aux("set", Set(split_int(set)), set)
               end
            end
            LL.S = LL_S
            local function LL_R(...)
               if select("#", ...) == 0 then
                  return LL_P(false)
               else
                  local range = Range(1, 0) --Set("")
                  for _, r in ipairs { ... } do
                     r = checkstring(r, "R")
                     assert(#r == 2, "bad argument #1 to 'R' (range must have two characters)")
                     range = S_union(range, Range(t_unpack(split_int(r))))
                  end
                  return constructors.aux("set", range)
               end
            end
            LL.R = LL_R
            local function LL_V(name)
               assert(name ~= nil)
               return constructors.aux("ref", name)
            end
            LL.V = LL_V
            do
               local one = setify { "set", "range", "one", "char" }
               local zero = setify { "true", "false", "lookahead", "unm" }
               local forbidden = setify {
                  "Carg",
                  "Cb",
                  "C",
                  "Cf",
                  "Cg",
                  "Cs",
                  "Ct",
                  "/zero",
                  "Clb",
                  "Cmt",
                  "Cc",
                  "Cp",
                  "div_string",
                  "div_number",
                  "div_table",
                  "div_function",
                  "at least",
                  "at most",
                  "behind",
               }
               local function fixedlen(pt, gram, cycle)
                  local typ = pt.pkind
                  if forbidden[typ] then
                     return false
                  elseif one[typ] then
                     return 1
                  elseif zero[typ] then
                     return 0
                  elseif typ == "string" then
                     return #pt.as_is
                  elseif typ == "any" then
                     return pt.aux
                  elseif typ == "choice" then
                     local l1, l2 = fixedlen(pt[1], gram, cycle), fixedlen(pt[2], gram, cycle)
                     return (l1 == l2) and l1
                  elseif typ == "sequence" then
                     local l1, l2 = fixedlen(pt[1], gram, cycle), fixedlen(pt[2], gram, cycle)
                     return l1 and l2 and l1 + l2
                  elseif typ == "grammar" then
                     if pt.aux[1].pkind == "ref" then
                        return fixedlen(pt.aux[pt.aux[1].aux], pt.aux, {})
                     else
                        return fixedlen(pt.aux[1], pt.aux, {})
                     end
                  elseif typ == "ref" then
                     if cycle[pt] then
                        return false
                     end
                     cycle[pt] = true
                     return fixedlen(gram[pt.aux], gram, cycle)
                  else
                     print(typ, "is not handled by fixedlen()")
                  end
               end
               function LL.B(pt)
                  pt = LL_P(pt)
                  local len = fixedlen(pt)
                  assert(len, "A 'behind' pattern takes a fixed length pattern as argument.")
                  if len >= 260 then
                     error "Subpattern too long in 'behind' pattern constructor."
                  end
                  return constructors.both("behind", pt, len)
               end
            end
            local function nameify(a, b)
               return tostring(a) .. tostring(b)
            end
            local function choice(a, b)
               local name = tostring(a) .. tostring(b)
               local ch = Builder.ptcache.choice[name]
               if not ch then
                  ch = factorize_choice(a, b) or constructors.binary("choice", a, b)
                  Builder.ptcache.choice[name] = ch
               end
               return ch
            end
            function LL.__add(a, b)
               return choice(LL_P(a), LL_P(b))
            end
            local function sequence(a, b)
               local name = tostring(a) .. tostring(b)
               local seq = Builder.ptcache.sequence[name]
               if not seq then
                  seq = factorize_sequence(a, b) or constructors.binary("sequence", a, b)
                  Builder.ptcache.sequence[name] = seq
               end
               return seq
            end
            Builder.sequence = sequence
            function LL.__mul(a, b)
               return sequence(LL_P(a), LL_P(b))
            end
            local function LL_lookahead(pt)
               if pt == truept or pt == falsept or pt.pkind == "unm" or pt.pkind == "lookahead" then
                  return pt
               end
               return constructors.subpt("lookahead", pt)
            end
            LL.__len = LL_lookahead
            LL.L = LL_lookahead
            local function LL_unm(pt)
               return factorize_unm(pt) or constructors.subpt("unm", pt)
            end
            LL.__unm = LL_unm
            local function LL_sub(a, b)
               a, b = LL_P(a), LL_P(b)
               return LL_unm(b) * a
            end
            LL.__sub = LL_sub
            local function LL_repeat(pt, n)
               local success
               success, n = pcall(tonumber, n)
               assert(success and type(n) == "number", "Invalid type encountered at right side of '^'.")
               return constructors.both((n < 0 and "at most" or "at least"), pt, n)
            end
            LL.__pow = LL_repeat
            for _, cap in pairs { "C", "Cs", "Ct" } do
               LL[cap] = function(pt)
                  pt = LL_P(pt)
                  return constructors.subpt(cap, pt)
               end
            end
            LL["Cb"] = function(aux)
               return constructors.aux("Cb", aux)
            end
            LL["Carg"] = function(aux)
               assert(type(aux) == "number", "Number expected as parameter to Carg capture.")
               assert(0 < aux and aux <= 200, "Argument out of bounds in Carg capture.")
               return constructors.aux("Carg", aux)
            end
            local function LL_Cp()
               return Cppt
            end
            LL.Cp = LL_Cp
            local function LL_Cc(...)
               return constructors.none("Cc", t_pack(...))
            end
            LL.Cc = LL_Cc
            for _, cap in pairs { "Cf", "Cmt" } do
               local msg = "Function expected in " .. cap .. " capture"
               LL[cap] = function(pt, aux)
                  assert(type(aux) == "function", msg)
                  pt = LL_P(pt)
                  return constructors.both(cap, pt, aux)
               end
            end
            local function LL_Cg(pt, tag)
               pt = LL_P(pt)
               if tag ~= nil then
                  return constructors.both("Clb", pt, tag)
               else
                  return constructors.subpt("Cg", pt)
               end
            end
            LL.Cg = LL_Cg
            local valid_slash_type = setify { "string", "number", "table", "function" }
            local function LL_slash(pt, aux)
               if LL_ispattern(aux) then
                  error "The right side of a '/' capture cannot be a pattern."
               elseif not valid_slash_type[type(aux)] then
                  error("The right side of a '/' capture must be of type " .. "string, number, table or function.")
               end
               local name
               if aux == 0 then
                  name = "/zero"
               else
                  name = "div_" .. type(aux)
               end
               return constructors.both(name, pt, aux)
            end
            LL.__div = LL_slash
            if Builder.proxymt then
               for k, v in pairs(LL) do
                  if k:match "^__" then
                     Builder.proxymt[k] = v
                  end
               end
            else
               LL.__index = LL
            end
            local factorizer = Builder.factorizer(Builder, LL)
            factorize_choice, factorize_lookahead, factorize_sequence, factorize_unm =
               factorizer.choice, factorizer.lookahead, factorizer.sequence, factorizer.unm
         end -- module wrapper --------------------------------------------------------
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["constructors"] = function(...)
         local getmetatable, ipairs, newproxy, print, setmetatable = getmetatable, ipairs, newproxy, print, setmetatable
         local t, u, compat = require "table", require "util", require "compat"
         local t_concat = t.concat
         local copy, getuniqueid, id, map, weakkey, weakval = u.copy, u.getuniqueid, u.id, u.map, u.weakkey, u.weakval
         local _ENV = u.noglobals() ----------------------------------------------------
         local patternwith = {
            constant = {
               "Cp",
               "true",
               "false",
            },
            aux = {
               "string",
               "any",
               "char",
               "range",
               "set",
               "ref",
               "sequence",
               "choice",
               "Carg",
               "Cb",
            },
            subpt = {
               "unm",
               "lookahead",
               "C",
               "Cf",
               "Cg",
               "Cs",
               "Ct",
               "/zero",
            },
            both = {
               "behind",
               "at least",
               "at most",
               "Clb",
               "Cmt",
               "div_string",
               "div_number",
               "div_table",
               "div_function",
            },
            none = "grammar",
            "Cc",
         }
         return function(Builder, LL) --- module wrapper.
            local S_tostring = Builder.set.tostring
            local newpattern, pattmt
            if compat.proxies and not compat.lua52_len then
               local proxycache = weakkey {}
               local __index_LL = { __index = LL }
               local baseproxy = newproxy(true)
               pattmt = getmetatable(baseproxy)
               Builder.proxymt = pattmt
               function pattmt:__index(k)
                  return proxycache[self][k]
               end
               function pattmt:__newindex(k, v)
                  proxycache[self][k] = v
               end
               function LL.getdirect(p)
                  return proxycache[p]
               end
               function newpattern(cons)
                  local pt = newproxy(baseproxy)
                  setmetatable(cons, __index_LL)
                  proxycache[pt] = cons
                  return pt
               end
            else
               if LL.warnings and not compat.lua52_len then
                  print(
                     "Warning: The `__len` metatethod won't work with patterns, "
                        .. "use `LL.L(pattern)` for lookaheads."
                  )
               end
               pattmt = LL
               function LL.getdirect(p)
                  return p
               end
               function newpattern(pt)
                  return setmetatable(pt, LL)
               end
            end
            Builder.newpattern = newpattern
            local function LL_ispattern(pt)
               return getmetatable(pt) == pattmt
            end
            LL.ispattern = LL_ispattern
            function LL.type(pt)
               if LL_ispattern(pt) then
                  return "pattern"
               else
                  return nil
               end
            end
            local ptcache, meta
            local function resetcache()
               ptcache, meta = {}, weakkey {}
               Builder.ptcache = ptcache
               for _, p in ipairs(patternwith.aux) do
                  ptcache[p] = weakval {}
               end
               for _, p in ipairs(patternwith.subpt) do
                  ptcache[p] = weakval {}
               end
               for _, p in ipairs(patternwith.both) do
                  ptcache[p] = {}
               end
               return ptcache
            end
            LL.resetptcache = resetcache
            resetcache()
            local constructors = {}
            Builder.constructors = constructors
            constructors["constant"] = {
               truept = newpattern { pkind = "true" },
               falsept = newpattern { pkind = "false" },
               Cppt = newpattern { pkind = "Cp" },
            }
            local getauxkey = {
               string = function(aux, as_is)
                  return as_is
               end,
               table = copy,
               set = function(aux, as_is)
                  return S_tostring(aux)
               end,
               range = function(aux, as_is)
                  return t_concat(as_is, "|")
               end,
               sequence = function(aux, as_is)
                  return t_concat(map(getuniqueid, aux), "|")
               end,
            }
            getauxkey.choice = getauxkey.sequence
            constructors["aux"] = function(typ, aux, as_is)
               local cache = ptcache[typ]
               local key = (getauxkey[typ] or id)(aux, as_is)
               if not cache[key] then
                  cache[key] = newpattern {
                     pkind = typ,
                     aux = aux,
                     as_is = as_is,
                  }
               end
               return cache[key]
            end
            constructors["none"] = function(typ, aux)
               return newpattern {
                  pkind = typ,
                  aux = aux,
               }
            end
            constructors["subpt"] = function(typ, pt)
               local cache = ptcache[typ]
               if not cache[pt] then
                  cache[pt] = newpattern {
                     pkind = typ,
                     pattern = pt,
                  }
               end
               return cache[pt]
            end
            constructors["both"] = function(typ, pt, aux)
               local cache = ptcache[typ][aux]
               if not cache then
                  ptcache[typ][aux] = weakval {}
                  cache = ptcache[typ][aux]
               end
               if not cache[pt] then
                  cache[pt] = newpattern {
                     pkind = typ,
                     pattern = pt,
                     aux = aux,
                     cache = cache, -- needed to keep the cache as long as the pattern exists.
                  }
               end
               return cache[pt]
            end
            constructors["binary"] = function(typ, a, b)
               return newpattern {
                  a,
                  b,
                  pkind = typ,
               }
            end
         end -- module wrapper
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["init"] = function(...)
         local getmetatable, setmetatable, pcall = getmetatable, setmetatable, pcall
         local u = require "util"
         local copy, map, nop, t_unpack = u.copy, u.map, u.nop, u.unpack
         local API, charsets, compiler, constructors, datastructures, evaluator, factorizer, locale, printers, re =
            t_unpack(map(require, {
               "API",
               "charsets",
               "compiler",
               "constructors",
               "datastructures",
               "evaluator",
               "factorizer",
               "locale",
               "printers",
               "re",
            }))
         local _, package = pcall(require, "package")
         local _ENV = u.noglobals() ----------------------------------------------------
         local VERSION = "0.12"
         local LuVERSION = "0.1.0"
         local function global(self, env)
            setmetatable(env, { __index = self })
         end
         local function register(self, env)
            pcall(function()
               package.loaded.lpeg = self
               package.loaded.re = self.re
            end)
            if env then
               env.lpeg, env.re = self, self.re
            end
            return self
         end
         local function LuLPeg(options)
            options = options and copy(options) or {}
            local Builder, LL =
               { options = options, factorizer = factorizer }, {
                  new = LuLPeg,
                  version = function()
                     return VERSION
                  end,
                  luversion = function()
                     return LuVERSION
                  end,
                  setmaxstack = nop, --Just a stub, for compatibility.
               }
            LL.util = u
            LL.global = global
            LL.register = register -- Decorate the LuLPeg object.
            charsets(Builder, LL)
            datastructures(Builder, LL)
            printers(Builder, LL)
            constructors(Builder, LL)
            API(Builder, LL)
            evaluator(Builder, LL);
            (options.compiler or compiler)(Builder, LL)
            locale(Builder, LL)
            LL.re = re(Builder, LL)
            return LL
         end -- LuLPeg
         local LL = LuLPeg()
         return LL
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["factorizer"] = function(...)
         local ipairs, pairs, print, setmetatable = ipairs, pairs, print, setmetatable
         local u = require "util"
         local id, nop, setify, weakkey = u.id, u.nop, u.setify, u.weakkey
         local _ENV = u.noglobals() ----------------------------------------------------
         local function process_booleans(a, b, opts)
            local id, brk = opts.id, opts.brk
            if a == id then
               return true, b
            elseif b == id then
               return true, a
            elseif a == brk then
               return true, brk
            else
               return false
            end
         end
         local unary = setify {
            "unm",
            "lookahead",
            "C",
            "Cf",
            "Cg",
            "Cs",
            "Ct",
            "/zero",
         }
         local unary_aux = setify {
            "behind",
            "at least",
            "at most",
            "Clb",
            "Cmt",
            "div_string",
            "div_number",
            "div_table",
            "div_function",
         }
         local unifiable = setify { "char", "set", "range" }
         local hasCmt
         hasCmt = setmetatable({}, {
            __mode = "k",
            __index = function(self, pt)
               local kind, res = pt.pkind, false
               if kind == "Cmt" or kind == "ref" then
                  res = true
               elseif unary[kind] or unary_aux[kind] then
                  res = hasCmt[pt.pattern]
               elseif kind == "choice" or kind == "sequence" then
                  res = hasCmt[pt[1]] or hasCmt[pt[2]]
               end
               hasCmt[pt] = res
               return res
            end,
         })
         return function(Builder, LL) --------------------------------------------------
            if Builder.options.factorize == false then
               return {
                  choice = nop,
                  sequence = nop,
                  lookahead = nop,
                  unm = nop,
               }
            end
            local constructors, LL_P = Builder.constructors, LL.P
            local truept, falsept = constructors.constant.truept, constructors.constant.falsept
            local Range, Set, S_union = Builder.Range, Builder.set.new, Builder.set.union
            local mergeable = setify { "char", "set" }
            local type2cons = {
               ["/zero"] = "__div",
               ["div_number"] = "__div",
               ["div_string"] = "__div",
               ["div_table"] = "__div",
               ["div_function"] = "__div",
               ["at least"] = "__exp",
               ["at most"] = "__exp",
               ["Clb"] = "Cg",
            }
            local function choice(a, b)
               do -- handle the identity/break properties of true and false.
                  local hasbool, res = process_booleans(a, b, { id = falsept, brk = truept })
                  if hasbool then
                     return res
                  end
               end
               local ka, kb = a.pkind, b.pkind
               if a == b and not hasCmt[a] then
                  return a
               elseif ka == "choice" then -- correct associativity without blowing up the stack
                  local acc, i = {}, 1
                  while a.pkind == "choice" do
                     acc[i], a, i = a[1], a[2], i + 1
                  end
                  acc[i] = a
                  for j = i, 1, -1 do
                     b = acc[j] + b
                  end
                  return b
               elseif mergeable[ka] and mergeable[kb] then
                  return constructors.aux("set", S_union(a.aux, b.aux))
               elseif mergeable[ka] and kb == "any" and b.aux == 1 or mergeable[kb] and ka == "any" and a.aux == 1 then
                  return ka == "any" and a or b
               elseif ka == kb then
                  if (unary[ka] or unary_aux[ka]) and (a.aux == b.aux) then
                     return LL[type2cons[ka] or ka](a.pattern + b.pattern, a.aux)
                  elseif (ka == kb) and ka == "sequence" then
                     if a[1] == b[1] and not hasCmt[a[1]] then
                        return a[1] * (a[2] + b[2])
                     end
                  end
               end
               return false
            end
            local function lookahead(pt)
               return pt
            end
            local function sequence(a, b)
               do
                  local hasbool, res = process_booleans(a, b, { id = truept, brk = falsept })
                  if hasbool then
                     return res
                  end
               end
               local ka, kb = a.pkind, b.pkind
               if ka == "sequence" then -- correct associativity without blowing up the stack
                  local acc, i = {}, 1
                  while a.pkind == "sequence" do
                     acc[i], a, i = a[1], a[2], i + 1
                  end
                  acc[i] = a
                  for j = i, 1, -1 do
                     b = acc[j] * b
                  end
                  return b
               elseif (ka == "one" or ka == "any") and (kb == "one" or kb == "any") then
                  return LL_P(a.aux + b.aux)
               end
               return false
            end
            local function unm(pt)
               if pt == truept then
                  return falsept
               elseif pt == falsept then
                  return truept
               elseif pt.pkind == "unm" then
                  return #pt.pattern
               elseif pt.pkind == "lookahead" then
                  return -pt.pattern
               end
            end
            return {
               choice = choice,
               lookahead = lookahead,
               sequence = sequence,
               unm = unm,
            }
         end
      end
   end
   --=============================================================================
   do
      local _ENV = _ENV
      packages["optimizer"] = function(...)
         -- Nothing for now.
      end
   end
   return require "init"
end
-- local lulpeg = require "init"

--                   The Romantic WTF public license.
--                   --------------------------------
--                   a.k.a. version "<3" or simply v3
--
--
--            Dear user,
--
--            The LuLPeg library
--
--                                             \
--                                              '.,__
--                                           \  /
--                                            '/,__
--                                            /
--                                           /
--                                          /
--                       has been          / released
--                  ~ ~ ~ ~ ~ ~ ~ ~       ~ ~ ~ ~ ~ ~ ~ ~
--                under  the  Romantic   WTF Public License.
--               ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~`,´ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--               I hereby grant you an irrevocable license to
--                ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--                  do what the gentle caress you want to
--                       ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--                           with   this   lovely
--                              ~ ~ ~ ~ ~ ~ ~ ~
--                               / library...
--                              /  ~ ~ ~ ~
--                             /    Love,
--                        #   /      ','
--                        #######    ·
--                        #####
--                        ###
--                        #
--
--               -- Pierre-Yves
--
--
--
--            P.S.: Even though I poured my heart into this work,
--                  I _cannot_ provide any warranty regarding
--                  its fitness for _any_ purpose. You
--                  acknowledge that I will not be held liable
--                  for any damage its use could incur.
--
-- -----------------------------------------------------------------------------
--
-- LuLPeg, Copyright (C) 2013 Pierre-Yves Gérardy.
--
-- The `re` module and lpeg.*.*.test.lua,
-- Copyright (C) 2013 Lua.org, PUC-Rio.
--
-- Permission is hereby granted, free of charge,
-- to any person obtaining a copy of this software and
-- associated documentation files (the "Software"),
-- to deal in the Software without restriction,
-- including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software,
-- and to permit persons to whom the Software is
-- furnished to do so,
-- subject to the following conditions:
--
-- The above copyright notice and this permission notice
-- shall be included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED,
-- INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

local function losc()
   local Serializer = {}

   --- Require a function for packing.
   -- @return A suitable packing function as explained in the header of this file.
   function Serializer.pack()
      if string.pack then
         return string.pack
      end
      local ok, _ = pcall(require, "struct")
      if ok then
         return require("struct").pack
      else
         --TODO:
         -- return require(relpath .. ".lib.struct").pack
      end
   end

   --- Require a function for unpacking.
   -- @return A suitable unpacking function as explained in the header of this file.
   function Serializer.unpack()
      if string.unpack then
         return string.unpack
      end
      local ok, _ = pcall(require, "struct")
      if ok then
         return require("struct").unpack
      else
         -- return require(relpath .. ".lib.struct").unpack
      end
   end

   local _pack = Serializer.pack()
   local _unpack = Serializer.unpack()
   local has_string_pack = string.pack and true or false

   local Timetag = {}

   -- 70 years in seconds (1970 - 1900)
   local NTP_SEC_OFFSET = 2208988800
   -- 2^32
   local TWO_POW_32 = 4294967296

   local function tt_add(timetag, seconds)
      local sec = math.floor(seconds)
      local frac = math.floor(TWO_POW_32 * (seconds - sec) + 0.5)
      sec = sec + timetag.content.seconds
      frac = frac + timetag.content.fractions
      return Timetag.new_raw(sec, frac)
   end

   Timetag.__index = Timetag

   --- Add a time offset to a Timetag.
   -- This overloads the `+` operator for Timetags and should not be called directly.
   -- @usage local tt = Timetag.new(os.time()) + 0.25
   Timetag.__add = function(a, b)
      if type(a) == "number" then
         return tt_add(b, a)
      end
      if type(b) == "number" then
         return tt_add(a, b)
      end
   end

   --- Low level API
   -- @section low-level-api

   --- Create a new Timetag.
   --
   -- @param[opt] tbl Table with timetag content.
   -- @param[opt] seconds Seconds since January 1st 1900 in the UTC timezone.
   -- @param[opt] fractions Fractions expressed as 1/2^32 of a second.
   --
   -- If there are no arguments a timetag with special value of "immediate" will be returned.
   function Timetag.new_raw(...)
      local self = setmetatable({}, Timetag)
      local args = { ... }
      -- 0x0000000000000001 equals "now", so this is the default.
      self.content = { seconds = 0, fractions = 1 }
      if #args >= 1 then
         if type(args[1]) == "table" then
            self.content = args[1]
         elseif type(args[1]) == "number" and not args[2] then
            self.content.seconds = args[1]
         elseif type(args[1]) == "number" and type(args[2]) == "number" then
            self.content.seconds = args[1]
            self.content.fractions = args[2]
         end
      end
      return self
   end

   local Types = {}

   --- Type pack functions.
   --
   -- Custom pack functions can be added to this table and standard functions can
   -- be re-assigned if necessary.
   --
   -- This table can be called to pack a value in protected mode (pcall).
   -- @usage local ok, data = Types.pack('s', 'hello')
   -- if ok then
   --   -- do something with data.
   -- end
   Types.pack = {}
   setmetatable(Types.pack, {
      __call = function(self, type, value)
         return pcall(self[type], value)
      end,
   })

   --- Type unpack functions.
   --
   -- Custom unpack functions can be added to this table and standard functions
   -- can be re-assigned if necessary.
   --
   -- This table can be called to unpack a value in protected mode (pcall).
   -- @usage local ok, value, index = Types.unpack('s', data, 1)
   -- if ok then
   --   -- do something with value/index.
   -- end
   Types.unpack = {}
   setmetatable(Types.unpack, {
      __call = function(self, type, data, offset)
         return pcall(self[type], data, offset)
      end,
   })

   --- Get available types.
   -- @tparam table tbl `Types.unpack` or `Types.pack`
   -- @return Table with available types.
   -- @usage local types = Types.types(Types.pack)
   -- @usage local types = Types.types(Types.unpack)
   function Types.get(tbl)
      local types = {}
      for k, _ in pairs(tbl) do
         types[#types + 1] = k
      end
      return types
   end

   local function strsize(s)
      return 4 * (math.floor(#s / 4) + 1)
   end

   local function blobsize(b)
      return 4 * (math.floor((#b + 3) / 4))
   end

   --- Atomic types.
   -- @section atomic-types

   --- 32-bit big-endian two's complement integer.
   -- @param value The value to pack.
   -- @return Binary string buffer.
   Types.pack.i = function(value)
      return _pack(">i4", value)
   end

   --- 32-bit big-endian two's complement integer.
   -- @param data The data to unpack.
   -- @param[opt] offset Initial offset into data.
   -- @return value, index of the bytes read + 1.
   Types.unpack.i = function(data, offset)
      return _unpack(">i4", data, offset)
   end

   --- 32-bit big-endian IEEE 754 floating point number.
   -- @param value The value to pack.
   -- @return Binary string buffer.
   Types.pack.f = function(value)
      return _pack(">f", value)
   end

   --- 32-bit big-endian IEEE 754 floating point number.
   -- @param data The data to unpack.
   -- @param[opt] offset Initial offset into data.
   -- @return value, index of the bytes read + 1.
   Types.unpack.f = function(data, offset)
      return _unpack(">f", data, offset)
   end

   --- String (null terminated)
   -- @param value The value to pack.
   -- @return Binary string buffer.
   Types.pack.s = function(value)
      local len = strsize(value)
      local fmt = "c" .. len
      value = value .. string.rep(string.char(0), len - #value)
      return _pack(">" .. fmt, value)
   end

   --- String (null terminated)
   -- @param data The data to unpack.
   -- @param[opt] offset Initial offset into data.
   -- @return value, index of the bytes read + 1.
   Types.unpack.s = function(data, offset)
      local fmt = has_string_pack and "z" or "s"
      local str = _unpack(">" .. fmt, data, offset)
      return str, strsize(str) + (offset or 1)
   end

   --- Blob (arbitrary binary data)
   -- @param value The value to pack.
   -- @return Binary string buffer.
   Types.pack.b = function(value)
      local size = #value
      local aligned = blobsize(value)
      local fmt = "c" .. aligned
      value = value .. string.rep(string.char(0), aligned - size)
      return _pack(">I4" .. fmt, size, value)
   end

   --- Blob (arbitrary binary data)
   -- @param data The data to unpack.
   -- @param[opt] offset Initial offset into data.
   -- @return value, index of the bytes read + 1.
   Types.unpack.b = function(data, offset)
      local size, blob
      size, offset = _unpack(">I4", data, offset)
      blob, offset = _unpack(">c" .. size, data, offset)
      return blob, offset + blobsize(blob) - size
   end

   --- Extended types.
   -- @section extended-types

   if has_string_pack then
      --- 64 bit big-endian two's complement integer.
      --
      -- **NOTE** This type is only supported for lua >= 5.3.
      -- @param value The value to pack.
      -- @return Binary string buffer.
      Types.pack.h = function(value)
         return _pack(">i8", value)
      end
   end

   if has_string_pack then
      --- 64 bit big-endian two's complement integer.
      --
      -- **NOTE** This type is only supported for lua >= 5.3.
      -- @param data The data to unpack.
      -- @param[opt] offset Initial offset into data.
      -- @return value, index of the bytes read + 1.
      Types.unpack.h = function(data, offset)
         return _unpack(">i8", data, offset)
      end
   end

   --- Timetag (64-bit integer divided into upper and lower part)
   -- @param value Table with seconds and fractions.
   -- @return Binary string buffer.
   -- @see losc.timetag
   Types.pack.t = function(value)
      return Timetag.pack(value)
   end

   --- Timetag (64-bit integer divided into upper and lower part)
   -- @param data The data to unpack.
   -- @param[opt] offset Initial offset into data.
   -- @return value, index of the bytes read + 1.
   -- @see losc.timetag
   Types.unpack.t = function(data, offset)
      return Timetag.unpack(data, offset)
   end

   --- 64-bit big-endian IEEE 754 floating point number.
   -- @param value The value to pack.
   -- @return Binary string buffer.
   Types.pack.d = function(value)
      return _pack(">d", value)
   end

   --- 64-bit big-endian IEEE 754 floating point number.
   -- @param data The data to unpack.
   -- @param[opt] offset Initial offset into data.
   -- @return value, index of the bytes read + 1.
   Types.unpack.d = function(data, offset)
      return _unpack(">d", data, offset)
   end

   --- Boolean true.
   -- This type does not have a corresponding `pack` method.
   -- @param _ Not used.
   -- @param[opt] offset Initial offset into data.
   -- @return true (boolean) and byte offset (not incremented).
   Types.unpack.T = function(_, offset)
      return true, offset or 0
   end

   --- Boolean false.
   -- This type does not have a corresponding `pack` method.
   -- @param _ Not used.
   -- @param[opt] offset Initial offset into data.
   -- @return false (boolean) and byte offset (not incremented).
   Types.unpack.F = function(_, offset)
      return false, offset or 0
   end

   --- Nil.
   -- This type does not have a corresponding `pack` method.
   -- @param _ Not used.
   -- @param[opt] offset Initial offset into data.
   -- @return false (since nil cannot be represented in a lua table) and byte offset (not incremented).
   Types.unpack.N = function(_, offset)
      -- TODO: decide on what to return here..
      return false, offset or 0
   end

   --- Infinitum.
   -- This type does not have a corresponding `pack` method.
   -- @param _ Not used.
   -- @param[opt] offset Initial offset into data.
   -- @return math.huge and byte offset (not incremented).
   Types.unpack.I = function(_, offset)
      return math.huge, offset or 0
   end
   -- local _pack = Serializer.pack()
   -- local _unpack = Serializer.unpack()

   --- High level API
   -- @section high-level-api

   --- New using a seconds and fractions.
   --
   -- Given nil arguments will return a timetag with special value "immediate".
   --
   -- @tparam[opt] integer seconds Timestamp seconds.
   -- @tparam[opt] integer fractions Timestamp fractions.
   -- @tparam[opt] integer precision The fraction precision. default 1000 (`milliseconds`)
   -- @usage local tt = Timetag.new() -- immediate
   -- @usage local tt = Timetag.new(os.time())
   -- @usage local tt = Timetag.new(tv.sec, tv.usec, 1e6)
   -- @see Timetag.new_raw
   function Timetag.new(seconds, fractions, precision)
      precision = precision or 1000
      if not seconds and not fractions then
         return Timetag.new_raw()
      end
      local secs, frac
      secs = (seconds or 0) + NTP_SEC_OFFSET
      frac = math.floor((fractions or 0) * (TWO_POW_32 / precision) + 0.5)
      return Timetag.new_raw(secs, frac)
   end

   --- Create a new OSC Timetag from a timestamp.
   --
   -- @param time The timestamp to use.
   -- @param[opt] precision The fraction precision. default 1000
   -- @return A Timetag object.
   -- @usage local tt = Timetag.new_from_timestamp(time)
   function Timetag.new_from_timestamp(time, precision)
      precision = precision or 1000
      local seconds = math.floor(time / precision)
      local fracs = math.floor(precision * (time / precision - seconds) + 0.5)
      return Timetag.new(seconds, fracs)
   end

   --- Get a timestamp value with arbitrary precision.
   -- @param precision The precision to use. default 1000 (`milliseconds`)
   -- @return Timestamp value.
   -- @usage
   -- local tt = Timetag.new(os.time(), 500)
   -- local timestamp = tt:timestamp()
   function Timetag:timestamp(precision)
      return Timetag.get_timestamp(self.content, precision)
   end

   --- Get seconds.
   -- @return Timetag seconds.
   function Timetag:seconds()
      return self.content.seconds
   end

   --- Get fractions.
   -- @return Timetag fractions.
   function Timetag:fractions()
      return self.content.fractions
   end

   --- Low level API
   -- @section low-level-api

   --- Get a timestamp with arbitrary precision.
   -- @param tbl Table with seconds and fractions.
   -- @param[opt] precision The fraction precision. default 1000
   -- @return Timetag value.
   function Timetag.get_timestamp(tbl, precision)
      precision = precision or 1000
      local seconds = precision * math.max(0, tbl.seconds - NTP_SEC_OFFSET)
      local fractions = math.floor(precision * (tbl.fractions / TWO_POW_32) + 0.5)
      return seconds + fractions
   end

   --- Pack an OSC Timetag.
   --
   -- The returned object is suitable for sending via a transport layer such as
   -- UDP or TCP.
   --
   -- @tparam table tbl The timetag to pack.
   -- @return OSC data packet (byte string).
   -- @usage
   -- local tt = {seconds = os.time(), fractions = 0}
   -- local data = Timetag.pack(tt)
   function Timetag.pack(tbl)
      local data = {}
      data[#data + 1] = _pack(">I4", tbl.seconds)
      data[#data + 1] = _pack(">I4", tbl.fractions)
      return table.concat(data, "")
   end

   --- Unpack an OSC Timetag.
   --
   -- @param data The data to unpack.
   -- @param offset The initial offset into data.
   -- @return First is a table with seconds and fractions, second is index of the bytes read + 1.
   function Timetag.unpack(data, offset)
      local seconds, fractions
      seconds, offset = _unpack(">I4", data, offset)
      fractions, offset = _unpack(">I4", data, offset)
      return { seconds = seconds, fractions = fractions }, offset
   end
   local Message = {}
   Message.__index = Message

   --- High level API
   -- @section high-level-api

   --- Create a new OSC message.
   --
   -- @tparam[opt] string|table msg OSC address or table constructor.
   --
   -- @return An OSC message object.
   -- @see losc.types
   -- @usage
   -- local msg = Message.new()
   -- @usage
   -- local msg = Message.new('/some/addr')
   -- @usage
   -- local tbl = {address = '/some/addr', types = 'ifs', 1, 2.0, 'hi'}
   -- local msg = Message.new(tbl)
   function Message.new(msg)
      local self = setmetatable({}, Message)
      self.content = {}
      self.content.address = ""
      self.content.types = ""
      if msg then
         if type(msg) == "string" then
            if msg:sub(1, 1) ~= "/" then
               msg = "/" .. msg
            end
            Message.address_validate(msg)
            self.content.address = msg
         elseif type(msg) == "table" then
            Message.tbl_validate(msg)
            self.content = msg
         end
      end
      return self
   end

   --- Add arguments to the message.
   --
   -- @param type OSC type string.
   -- @param[opt] item Item to add.
   -- @see losc.types
   -- @usage message:add('i', 123)
   -- @usage message:add('T')
   function Message:add(type, item)
      self.content.types = self.content.types .. type
      if item then
         self.content[#self.content + 1] = item
      else
         if type == "T" or type == "F" then
            self.content[#self.content + 1] = type == "T"
         elseif type == "N" then
            self.content[#self.content + 1] = false
         elseif type == "I" then
            self.content[#self.content + 1] = math.huge
         end
      end
   end

   --- Message iterator.
   --
   -- Iterate over message types and arguments.
   --
   -- @return iterator using index, type, argument.
   -- @usage for i, type, arg in message:iter() do
   --   print(i, type, arg)
   -- end
   function Message:iter()
      local tbl = { self.content.types, self.content }
      local function msg_it(t, i)
         i = i + 1
         local type = t[1]:sub(i, i)
         local arg = t[2][i]
         if type ~= nil and arg ~= nil then
            return i, type, arg
         end
         return nil
      end
      return msg_it, tbl, 0
   end

   --- Get the OSC address.
   -- @return The OSC address.
   function Message:address()
      return self.content.address
   end

   --- Get the OSC type string.
   -- @return OSC type string or empty string.
   function Message:types()
      return self.content.types
   end

   --- Get the OSC arguments.
   -- @return Table with arguments.
   function Message:args()
      local args = {}
      for _, a in ipairs(self.content) do
         args[#args + 1] = a
      end
      return args
   end

   --- Validate a message.
   -- @tparam table|string message The message to validate. Can be in packed or unpacked form.
   function Message.validate(message)
      assert(message)
      if type(message) == "string" then
         Message.bytes_validate(message)
      elseif type(message) == "table" then
         Message.tbl_validate(message.content or message)
      end
   end

   --- Low level API
   -- @section low-level-api

   --- Validate an OSC message address.
   -- @tparam string addr The address to validate.
   function Message.address_validate(addr)
      assert(not addr:find "[%s#*,%[%]{}%?]", "Invalid characters found in address.")
   end

   --- Validate a table to be used as a message constructor.
   -- @tparam table tbl The table to validate.
   function Message.tbl_validate(tbl)
      assert(tbl.address, 'Missing "address" field.')
      Message.address_validate(tbl.address)
      assert(tbl.types, 'Missing "types" field.')
      assert(#tbl.types == #tbl, "Types and arguments mismatch")
   end

   --- Validate a binary string to see if it is a valid OSC message.
   -- @tparam string bytes The byte string to validate.
   -- @tparam[opt] integer offset Byte offset.
   function Message.bytes_validate(bytes, offset)
      local value
      assert(#bytes % 4 == 0, "OSC message data must be a multiple of 4.")
      value, offset = Types.unpack.s(bytes, offset)
      assert(value:sub(1, 1) == "/", "Invalid OSC address.")
      value = Types.unpack.s(bytes, offset)
      assert(value:sub(1, 1) == ",", "Error: malformed type tag.")
   end

   --- Pack a table to a byte string.
   --
   -- The returned object is suitable for sending via a transport layer such as
   -- UDP or TCP.
   --
   -- Call `Message.validate()` before passing arguments to this function to
   -- ensure that the table is suitable for packing.
   --
   -- @param tbl The content to pack.
   -- @return OSC data packet (byte string).
   function Message.pack(tbl)
      local packet = {}
      local address = tbl.address
      local types = tbl.types
      -- types
      packet[#packet + 1] = Types.pack.s(address)
      packet[#packet + 1] = Types.pack.s("," .. types)
      -- arguments
      local index = 1
      for type in types:gmatch "." do
         local item = tbl[index]
         if item ~= nil then
            if Types.pack[type] then
               packet[#packet + 1] = Types.pack[type](item)
            end
            index = index + 1
         end
      end
      return table.concat(packet, "")
   end

   --- Unpack OSC message byte string.
   --
   -- Call `Message.validate()` before passing arguments to this function to
   -- ensure that the table is suitable for unpacking.
   --
   -- @param data The data to unpack.
   -- @param offset The initial offset into data.
   -- @return table with the content of the OSC message.
   function Message.unpack(data, offset)
      local value
      local message = {}
      -- address
      value, offset = Types.unpack.s(data, offset)
      message.address = value
      -- type tag
      value, offset = Types.unpack.s(data, offset)
      local types = value:sub(2) -- remove prefix
      message.types = types
      -- arguments
      for type in types:gmatch "." do
         if Types.unpack[type] then
            value, offset = Types.unpack[type](data, offset)
            message[#message + 1] = value
         end
      end
      return message, offset
   end

   local Bundle = {}
   Bundle.__index = Bundle

   local ts = Timetag.get_timestamp

   --- Pack a Bundle recursively.
   local function _pack(bundle, packet)
      packet[#packet + 1] = Types.pack.s "#bundle"
      packet[#packet + 1] = Types.pack.t(bundle.timetag)
      for _, item in ipairs(bundle) do
         if item.address and item.types then
            local message = Message.pack(item)
            packet[#packet + 1] = Types.pack.i(#message)
            packet[#packet + 1] = message
         elseif item.timetag then
            if ts(item.timetag) < ts(bundle.timetag) then
               error "Bundle timetag is less than enclosing bundle."
            end
            local bndl = Bundle.pack(item)
            packet[#packet + 1] = Types.pack.i(#bndl)
            packet[#packet + 1] = bndl
         end
      end
      return table.concat(packet, "")
   end

   --- Unpack a Bundle recursively.
   local function _unpack(data, bundle, offset, length)
      local value, _
      _, offset = Types.unpack.s(data, offset)
      value, offset = Types.unpack.t(data, offset)
      bundle.timetag = value
      length = length or #data
      while offset < length do
         -- content length
         value, offset = Types.unpack.i(data, offset)
         local head = data:sub(offset, offset)
         if head == "#" then
            value, offset = _unpack(data, {}, offset, offset + value - 1)
            bundle[#bundle + 1] = value
         elseif head == "/" then
            value, offset = Message.unpack(data, offset)
            bundle[#bundle + 1] = value
         end
      end
      return bundle, offset
   end

   --- High level API
   -- @section high-level-api

   --- Create a new OSC bundle.
   --
   -- Arguments can be one form of:
   --
   -- 1. nil (return empty bundle object).
   -- 2. Timetag.
   -- 3. Timetag, message/bundle objects.
   --
   -- @param[opt] ... arguments.
   -- @return Bundle object.
   function Bundle.new(...)
      local self = setmetatable({}, Bundle)
      local args = { ... }
      self.content = {}
      if #args >= 1 then
         self.content.timetag = args[1].content
         for index = 2, #args do
            self:add(args[index])
         end
      end
      return self
   end

   --- Adds an item to the bundle.
   -- @param item Can be a Message or another bundle.
   function Bundle:add(item)
      self.content[#self.content + 1] = item.content
   end

   --- Get or set the bundle Timetag.
   -- @param[opt] tt A Timetag object.
   -- If no parameter is given it returns the current Timetag.
   function Bundle:timetag(tt)
      if tt then
         self.content.timetag = tt.content
      else
         return Timetag.new_raw(self.content.timetag)
      end
   end

   --- Validate a bundle.
   -- @tparam table|string bundle The bundle to validate. Can be in packed or unpacked form.
   function Bundle.validate(bundle)
      assert(bundle)
      if type(bundle) == "string" then
         Bundle.bytes_validate(bundle)
      elseif type(bundle) == "table" then
         Bundle.tbl_validate(bundle.content or bundle)
      end
   end

   --- Low level API
   -- @section low-level-api

   --- Validate a table that can be used as an OSC bundle.
   -- @param tbl The table to validate.
   function Bundle.tbl_validate(tbl)
      assert(type(tbl.timetag) == "table", "Missing OSC Timetag.")
   end

   --- Validate a byte string that can be unpacked to an OSC bundle.
   -- @param data The byte string to validate.
   -- @param[opt] offset Byte offset.
   function Bundle.bytes_validate(data, offset)
      local value
      assert(#data % 4 == 0, "OSC bundle data must be a multiple of 4.")
      value, offset = Types.unpack.s(data, offset or 1)
      assert(value == "#bundle", "Missing bundle marker")
      value = Types.unpack.t(data, offset)
      assert(type(value) == "table", "Missing bundle timetag")
   end

   --- Pack an OSC bundle.
   --
   -- The returned object is suitable for sending via a transport layer such as
   -- UDP or TCP.
   --
   -- @param tbl The content to pack.
   -- @return OSC data packet (byte string).
   function Bundle.pack(tbl)
      local packet = {}
      return _pack(tbl, packet)
   end

   --- Unpack an OSC bundle byte string.
   --
   -- @param data The data to unpack.
   -- @param offset The initial offset into data.
   -- @return table with the content of the OSC bundle.
   function Bundle.unpack(data, offset)
      local bundle = {}
      return _unpack(data, bundle, offset or 1)
   end

   local Packet = {}

   --- Check if a packet is a bundle or a message.
   -- @tparam string|table packet The packet to check.
   -- @return True if packet is a bundle otherwise false.
   function Packet.is_bundle(packet)
      if type(packet) == "string" then
         local value = Types.unpack.s(packet)
         return value == "#bundle"
      elseif type(packet) == "table" then
         packet = packet.content or packet
         return type(packet.timetag) == "table"
      end
   end

   --- Validate a packet. Can be a message or a bundle.
   -- @tparam string|table packet The packet to validate.
   function Packet.validate(packet)
      if Packet.is_bundle(packet) then
         Bundle.validate(packet)
      else
         Message.validate(packet)
      end
   end

   --- Pack a Bundle or Message.
   -- @param tbl The table to pack.
   -- @return OSC data packet (byte string).
   function Packet.pack(tbl)
      if Packet.is_bundle(tbl) then
         Bundle.validate(tbl)
         return Bundle.pack(tbl.content or tbl)
      else
         Message.validate(tbl)
         return Message.pack(tbl.content or tbl)
      end
   end

   --- Unpack an OSC packet.
   -- @tparam string data The data to unpack.
   -- @return table with the content of the OSC message (bundle or message).
   function Packet.unpack(data)
      if Packet.is_bundle(data) then
         Bundle.validate(data)
         return Bundle.unpack(data)
      else
         Message.validate(data)
         return Message.unpack(data)
      end
   end

   local Pattern = {}

   -- local ts = Timetag.get_timestamp

   --- Escape magic lua characters from a pattern.
   -- @tparam string pattern The pattern to escape.
   -- @return A string with all magic lua characters escaped and OSC wildcards
   -- converted to lua pattern matching wildcards.
   function Pattern.escape(pattern)
      -- escape lua magic chars (order matters)
      pattern = pattern:gsub("%%", "%%%%")
      pattern = pattern:gsub("%.", "%%.")
      pattern = pattern:gsub("%(", "%%(")
      pattern = pattern:gsub("%)", "%%)")
      pattern = pattern:gsub("%+", "%%+")
      pattern = pattern:gsub("%$", "%%$")
      -- convert osc wildcards to lua patterns
      pattern = pattern:gsub("%*", ".*")
      pattern = pattern:gsub("%?", ".")
      pattern = pattern:gsub("%[!", "[^")
      pattern = pattern:gsub("%]", "]+")
      return pattern
   end

   local function match(key, address)
      local result = address:match(key) == address
      -- try and match group instead
      if not result and key:find "{" then
         local index = 1
         local tmps = ""
         for c in key:gmatch "." do
            local a = address:sub(index, index)
            if a == c then
               tmps = tmps .. c
               index = index + 1
            end
         end
         result = tmps == address
      end
      return result
   end

   local function invoke(message, timestamp, plugin)
      local address = message.address
      local now = plugin:now():timestamp(plugin.precision)
      local ignore_late = plugin.options.ignore_late or false
      if ignore_late and timestamp > 0 and timestamp < now then
         return
      end
      if plugin.handlers then
         for _, handler in pairs(plugin.handlers) do
            if match(handler.pattern, address) then
               plugin:schedule(timestamp - now, function()
                  handler.callback {
                     timestamp = now,
                     message = message,
                     remote_info = plugin.remote_info or {},
                  }
               end)
            end
         end
      end
   end

   local function dispatch(packet, plugin)
      if Packet.is_bundle(packet) then
         for _, item in ipairs(packet) do
            if Packet.is_bundle(item) then
               if ts(item.timetag, plugin.precision) >= ts(packet.timetag, plugin.precision) then
                  dispatch(item, plugin)
               else
                  error "Bundle timestamp is older than timestamp of enclosing bundle"
               end
            else
               invoke(item, ts(packet.timetag, plugin.precision), plugin)
            end
         end
      else
         invoke(packet, 0, plugin)
      end
   end

   --- Dispatch OSC packets.
   -- @tparam string data Packed OSC data byte string.
   -- @tparam table plugin The plugin to dispatch the message through.
   function Pattern.dispatch(data, plugin)
      local packet = Packet.unpack(data)
      dispatch(packet, plugin)
   end

   local losc = {
      _VERSION = "losc v1.0.1",
      _URL = "https://github.com/davidgranstrom/losc",
      _DESCRIPTION = "Open Sound Control (OSC) library for lua/luajit.",
      _LICENSE = [[
    MIT License

    Copyright (c) 2021 David Granström

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
  ]],
   }
   losc.__index = losc

   --- Create a new instance.
   -- @tparam[options] table options Options.
   -- @usage local osc = losc.new()
   -- @usage local osc = losc.new {plugin = plugin.new()}
   function losc.new(options)
      local self = setmetatable({}, losc)
      self.handlers = {}
      if options then
         if options.plugin then
            self:use(options.plugin)
         end
      end
      return self
   end

   --- Create a new Message.
   -- @tparam[opt] string|table args OSC address or table constructor.
   -- @return message object
   -- @see losc.message
   -- @usage local message = losc.new_message()
   -- @usage local message = losc.new_message('/address')
   -- @usage local message = losc.new_message{address = '/test', types = 'iif', 1, 2, 3}
   function losc.new_message(args)
      local ok, message = pcall(Message.new, args)
      if not ok then
         error(message)
      end
      return message
   end

   --- Create a new OSC bundle.
   -- @param[opt] ... arguments.
   -- @return bundle object
   -- @see losc.bundle
   -- @usage
   -- local tt = losc:now()
   -- local bundle = losc.new_bundle()
   -- bundle:timetag(tt)
   -- -- packet can be a message or another bundle
   -- bundle:add(packet)
   -- @usage
   -- local tt = losc:now()
   -- local bundle = losc.new_bundle(tt)
   -- bundle:add(packet)
   -- @usage
   -- local tt = losc:now()
   -- local bundle = losc.new_bundle(tt, packet, packet2)
   function losc.new_bundle(...)
      local ok, bundle = pcall(Bundle.new, ...)
      if not ok then
         error(bundle)
      end
      return bundle
   end

   --- Specify a plugin to use as transport layer.
   -- @param plugin The plugin to use, pass nil to disable current plugin.
   function losc:use(plugin)
      self.plugin = plugin
      if plugin then
         self.plugin.handlers = self.handlers
      end
   end

   --- Get an OSC timetag with the current timestamp.
   -- Will fall back to `os.time()` if `now()` is not implemented by the plugin
   -- in use.
   -- @usage local tt = losc:now()
   -- @usage
   -- -- 0.25 seconds into the future.
   -- local tt = losc:now() + 0.25
   function losc:now()
      if self.plugin and self.plugin.now then
         return self.plugin:now()
      end
      return Timetag.new(os.time(), 0)
   end

   --- Opens an OSC server.
   -- This function might be blocking depending on the plugin in use.
   -- @param[opt] ... Plugin specific arguments.
   -- @return status, plugin handle or error
   -- @usage losc:open()
   function losc:open(...)
      if not self.plugin then
         error '"open" must be implemented using a plugin.'
      end
      return pcall(self.plugin.open, self.plugin, ...)
   end

   --- Closes an OSC server.
   -- @param[opt] ... Plugin specific arguments.
   -- @return status, nil or error
   -- @usage losc:close()
   function losc:close(...)
      if not self.plugin then
         error '"close" must be implemented using a plugin.'
      end
      return pcall(self.plugin.close, self.plugin, ...)
   end

   --- Send an OSC packet.
   -- @param[opt] ... Plugin specific arguments.
   -- @return status, nil or error
   -- @usage
   -- -- can be message or bundle.
   -- local packet = losc.new_message{address = '/x', types = 'i', 1}
   -- losc:send(packet)
   -- -- additional plugin arguments (can vary between plugins)
   -- losc:send(packet, 'localhost', 9000)
   function losc:send(...)
      if not self.plugin then
         error '"send" must be implemented using a plugin.'
      end
      return pcall(self.plugin.send, self.plugin, ...)
   end

   --- Add an OSC handler.
   -- @param pattern The pattern to match on.
   -- @param func The callback to run if a message is received.
   -- The callback will get a single argument `data` from where the messsage can be retrived.
   -- @usage
   -- osc:add_handler('/pattern', function(data)
   --   -- message table, can be converted to Message if needed.
   --   local message = data.message
   --   -- timestamp when message was received, can be converted to Timetag if needed.
   --   local timestamp = data.timestamp
   --   -- table with remote (sender) info, can be empty depending on plugin.
   --   local remote_info = data.remote_info
   -- end)
   -- @usage
   -- osc:add_handler('/pattern', function(data)
   --   -- arguments can be accessed by index from the message table
   --   local arg1 = data.message[1]
   --   local arg2 = data.message[2]
   --   -- iterate over incoming OSC arguments
   --   for _, argument in ipairs(data.message) do
   --     print(argument)
   --   end
   -- end)
   -- @usage
   -- -- Pattern matching (groups)
   -- osc:add_handler('/param/{x,y}/123', function(data) end)
   -- -- Pattern matching (sequence)
   -- osc:add_handler('/param/[a-f]/123', function(data) end)
   -- -- Pattern matching (sequence)
   -- osc:add_handler('/param/[!a-f]/123', function(data) end)
   -- -- Pattern matching (wildcard)
   -- osc:add_handler('/param/*/123', function(data) end)
   -- osc:add_handler('*', function(data) end)
   function losc:add_handler(pattern, func)
      self.handlers[pattern] = {
         pattern = Pattern.escape(pattern),
         callback = func,
      }
      if self.plugin then
         self.plugin.handlers = self.handlers
      end
   end

   --- Remove an OSC handler.
   -- @param pattern The pattern for the handler to remove.
   -- @usage losc:remove_handler('/handler/to/remove')
   function losc:remove_handler(pattern)
      self.handlers[pattern] = nil
      if self.plugin then
         self.plugin.handlers[pattern] = nil
      end
   end

   --- Remove all registered OSC handlers.
   -- @usage losc:remove_all()
   function losc:remove_all()
      self.handlers = {}
      if self.plugin then
         self.plugin.handlers = {}
      end
   end
   losc.Timetag = Timetag
   losc.Bundle = Bundle
   losc.Message = Message
   losc.Pattern = Pattern
   losc.Packet = Packet

   return losc
end
--[[lit-meta
  name = "noearc/modal"
  version = "0.0.1.1"
  homepage = "https://github.com/noearc/modal"
  description = "tidal cycles in lua!"
  license = "GPL3"
]]
local ut = {}
local pattern = {}
local control = {}
local types = {}
local theory = {}
local notation = {}
local a2s = {}
local factory = {}
local uv = require"luv" or vim.uv
local has_lpeg, lpeg = pcall(require, "lpeg")
lpeg = has_lpeg and lpeg or lulpeg():register(not _ENV and _G)
local has_socket, socket = pcall(require, "socket")
local has_al, al = pcall(require, "abletonlink")
losc = losc()
local Timetag = losc.Timetag
local Pattern = losc.Pattern
local Packet = losc.Packet
local has_plugin, plugin = pcall(require, "losc.plugins.udp-socket")
_G.struct = nil
local has_RL, RL = pcall(require, "readline")

do
   local pairs = pairs
   local ipairs = ipairs
   local tostring = tostring
   local loadstring = loadstring or load
   local setmetatable = setmetatable
   local type = type
   local unpack = unpack or rawget(table, "unpack")
   local str_dump = string.dump
   local str_char = string.char
   local tconcat = table.concat
   local tremove = table.remove
   local floor = math.floor
   local ceil = math.ceil
   local abs = math.abs
   local huge = math.huge
   local d_getinfo = debug.getinfo
   local d_getlocal = debug.getlocal
   local d_sethook = debug.sethook
   local d_gethook = debug.gethook
   local d_getupvalue = debug.getupvalue
   local d_setupvalue = debug.setupvalue
   
   ut.Usecolor = true
   
   local envs = { "vim", "norns", "love", "pd" }
   
   for _, v in pairs(envs) do
      if rawget(_G, v) then
         ut.Usecolor = false
      end
   end
   
   -- from https://www.lua.org/gems/sample.pdf
   -- TODO: smarter cache over time maybe
   local function memoize(f)
      local mem = {} -- memoizing table
      setmetatable(mem, { __mode = "kv" }) -- make it weak
      return function(x) -- new version of ’f’, with memoizing
         local r = mem[x]
         if r == nil then -- no previous result?
            r = f(x) -- calls original function
            mem[x] = r -- store result for reuse
         end
         return r
      end
   end
   ut.memoize = memoize
   ut.loadstring = memoize(loadstring)
   
   ---@table term colors
   local colors = {}
   
   local colormt = {}
   
   function colormt:__tostring()
      return self.value
   end
   
   function colormt:__concat(other)
      return tostring(self) .. tostring(other)
   end
   
   function colormt:__call(s)
      return self .. s .. colors.reset
   end
   
   local function makecolor(value)
      return setmetatable({ value = str_char(27) .. "[" .. tostring(value) .. "m" }, colormt)
   end
   
   local colorvalues = {
      -- attributes
      reset = 0,
      clear = 0,
      default = 0,
      bright = 1,
      dim = 2,
      underscore = 4,
      blink = 5,
      reverse = 7,
      hidden = 8,
   
      -- foreground
      black = 30,
      red = 31,
      green = 32,
      yellow = 33,
      blue = 34,
      magenta = 35,
      cyan = 36,
      white = 37,
   
      -- background
      onblack = 40,
      onred = 41,
      ongreen = 42,
      onyellow = 43,
      onblue = 44,
      onmagenta = 45,
      oncyan = 46,
      onwhite = 47,
   }
   
   for c, v in pairs(colorvalues) do
      colors[c] = makecolor(v)
   end
   
   ut.colors = colors
   
   ---@table bitwise ops
   local bit = {}
   local MOD = 2 ^ 32
   local MODM = MOD - 1
   
   -- TODO: replace with memoize ...?
   local bit_memo = function(f)
      local mt = {}
      local t = setmetatable({}, mt)
      mt.__index = function(self, k)
         local v = f(k)
         self.k = v
         return v
      end
      return t
   end
   
   local make_bitop_uncached = function(t, m)
      local bitop = function(a, b)
         local res, p = 0, 1
         while a ~= 0 and b ~= 0 do
            local am, bm = a % m, b % m
            res = res + t[am][bm] * p
            a = (a - am) / m
            b = (b - bm) / m
            p = p * m
         end
         res = res + (a + b) * p
         return res
      end
      return bitop
   end
   
   local make_bitop = function(t)
      local op1 = make_bitop_uncached(t, 2 ^ 1)
      local op2 = bit_memo(function(a)
         return bit_memo(function(b)
            return op1(a, b)
         end)
      end)
      return make_bitop_uncached(op2, 2 ^ (t.n or 1))
   end
   
   local tobit = function(x)
      return x % 2 ^ 32
   end
   bit.tobit = tobit
   
   local bxor = make_bitop {
      [0] = { [0] = 0, [1] = 1 },
      [1] = { [0] = 1, [1] = 0 },
      n = 4,
   }
   bit.bxor = bxor
   
   local bnot = function(a)
      return MODM - a
   end
   bit.bnot = bnot
   
   local band = function(a, b)
      return ((a + b) - bxor(a, b)) / 2
   end
   bit.band = band
   
   local bor = function(a, b)
      return MODM - band(MODM - a, MODM - b)
   end
   bit.bor = bor
   
   local lshift, rshift
   rshift = function(a, disp)
      if disp < 0 then
         return lshift(a, -disp)
      end
      return floor(a % 2 ^ 32 / 2 ^ disp)
   end
   bit.rshift = rshift
   
   lshift = function(a, disp)
      if disp < 0 then
         return rshift(a, -disp)
      end
      return (a * 2 ^ disp) % 2 ^ 32
   end
   bit.lshift = lshift
   
   ut.bit = bit
   
   ---Copyright (c) 2016 rxi
   ---@table log
   local log = { _version = "0.1.0" }
   ut.log = log
   
   log.usecolor = true
   log.outfile = nil
   log.level = "trace"
   
   local modes = {
      { name = "trace", color = "\27[34m" },
      { name = "debug", color = "\27[36m" },
      { name = "info", color = "\27[32m" },
      { name = "warn", color = "\27[33m" },
      { name = "error", color = "\27[31m" },
      { name = "fatal", color = "\27[35m" },
   }
   
   local levels = {}
   for i, v in ipairs(modes) do
      levels[v.name] = i
   end
   
   local round = function(x, increment)
      increment = increment or 1
      x = x / increment
      return (x > 0 and floor(x + 0.5) or ceil(x - 0.5)) * increment
   end
   
   local _tostring = tostring
   
   local tostring = function(...)
      local t = {}
      for i = 1, select("#", ...) do
         local x = select(i, ...)
         if type(x) == "number" then
            x = round(x, 0.01)
         end
         t[#t + 1] = _tostring(x)
      end
      return table.concat(t, " ")
   end
   
   for i, x in ipairs(modes) do
      local nameupper = x.name:upper()
      log[x.name] = function(...)
         -- Return early if we're below the log level
         if i < levels[log.level] then
            return
         end
   
         local msg = tostring(...)
         local info = debug.getinfo(2, "Sl")
         local lineinfo = info.short_src .. ":" .. info.currentline
   
         -- Output to console
         print(
            string.format(
               "%s[%-6s%s]%s %s: %s",
               log.usecolor and x.color or "",
               nameupper,
               os.date "%H:%M:%S",
               log.usecolor and "\27[0m" or "",
               lineinfo,
               msg
            )
         )
   
         -- Output to log file
         if log.outfile then
            local fp = io.open(log.outfile, "a")
            local str = string.format("[%-6s%s] %s: %s\n", nameupper, os.date(), lineinfo, msg)
            fp:write(str)
            fp:close()
         end
      end
   end
   
   -- general utilities
   
   local function id(x)
      return x
   end
   ut.id = id
   
   function ut.is_array(tbl)
      return type(tbl) == "table" and (#tbl > 0 or next(tbl) == nil)
   end
   
   ---return size of hash table
   ---@param t table
   local tsize = function(t)
      local size = 0
      for _ in pairs(t) do
         size = size + 1
      end
      return size
   end
   
   ---structually compare two table, TODO: needed?
   ---@param rhs table
   ---@param lhs table
   ---@return boolean
   function ut.compare(rhs, lhs)
      if type(lhs) ~= type(rhs) then
         return false
      end
      if type(lhs) == "table" then
         if tsize(lhs) ~= tsize(rhs) then
            return false
         end
         for k, v in pairs(lhs) do
            local equal = ut.compare(v, rhs[k])
            if not equal then
               return false
            end
         end
      else
         return rhs == lhs
      end
      return true
   end
   
   ---@param value any
   ---@return string
   function ut.T(value)
      local base_type = type(value)
      if base_type == "table" then
         local cls = value.__class
         if cls then
            return cls
         end
      end
      return base_type
   end
   
   function ut.flatten(t)
      local flat = {}
      for i = 1, #t do
         local value = t[i]
         if ut.T(value) == "table" then
            local list = ut.flatten(value)
            for j = 1, #list do
               flat[#flat + 1] = list[j]
            end
         else
            flat[#flat + 1] = value
         end
      end
      return flat
   end
   
   ---list filter
   ---@param f function
   ---@param list table
   ---@return table
   function ut.filter(f, list)
      local res = {}
      for i = 1, #list do
         if f(list[i]) then
            res[#res + 1] = list[i]
         end
      end
      return res
   end
   
   local function reduce(f, acc, list)
      for i = 1, #list do
         acc = f(acc, list[i])
      end
      return acc
   end
   ut.reduce = reduce
   
   ---list map
   ---@param f function
   ---@param list table
   ---@return table
   function ut.map(f, list)
      for i = 1, #list do
         list[i] = f(list[i], i)
      end
      return list
   end
   
   function ut.dumpf(f)
      local fmt = "fun(%s)"
      local args = ut.get_args(f)
      local argstr = tconcat(args, ", ")
      return fmt:format(argstr)
   end
   
   ---dump table as key value pairs
   ---@param o table
   ---@return string
   function ut.tdump(o)
      if ut.T(o) == "table" then
         local s = {}
         for k, v in pairs(o) do
            s[#s + 1] = k
            s[#s + 1] = ": "
            s[#s + 1] = ut.tdump(v)
            s[#s + 1] = " "
         end
         return tconcat(s)
      elseif ut.T(o) == "string" then
         local str = '"' .. o .. '"'
         return ut.Usecolor and ut.colors.green(str) or str
      elseif ut.T(o) == "number" then
         return tostring(ut.Usecolor and ut.colors.yellow(o) or o)
      elseif ut.T(o) == "function" then
         return ut.Usecolor and ut.colors.blue(ut.dumpf(o)) or ut.dumpf(o)
      else
         return tostring(ut.Usecolor and ut.colors.red(o) or o)
      end
   end
   
   ---dump table of events the tidal way
   ---@param o table
   ---@return string
   function ut.dump(o)
      if ut.T(o) == "table" then
         local s = {}
         for k, v in pairs(o) do
            s[#s + 1] = ut.Usecolor and ut.colors.cyan(k) or k
            s[#s + 1] = ": "
            s[#s + 1] = ut.dump(v)
            s[#s + 1] = (k ~= #o) and "\n" or ""
         end
         return tconcat(s)
      elseif ut.T(o) == "string" then
         local str = '"' .. o .. '"'
         return ut.Usecolor and ut.colors.green(str) or str
      elseif ut.T(o) == "number" then
         return tostring(ut.Usecolor and ut.colors.yellow(o) or o)
      elseif ut.T(o) == "function" then
         return ut.Usecolor and ut.colors.blue(ut.dumpf(o)) or ut.dumpf(o)
      else
         return tostring(o)
      end
   end
   
   ---zip two list (xs, ys) with f(xs, ys)
   ---@param f function
   ---@param xs table
   ---@param ys table
   ---@return table
   function ut.zipWith(f, xs, ys)
      local acc = {}
      for i = 1, #xs do
         acc[i] = f(xs[i], ys[i])
      end
      return acc
   end
   
   ---concat two lists
   ---@param a table
   ---@param b table
   ---@return table
   local function concat(a, b)
      for i = 1, #b do
         a[#a + 1] = b[i]
      end
      -- return chain(a, b):totable()
      return a
   end
   ut.concat = concat
   
   ---concat two hashmaps
   ---@param a table
   ---@param b table
   ---@return table
   function ut.union(a, b)
      for k, v in pairs(b) do
         a[k] = v
      end
      return a
   end
   
   ---@param index number
   ---@param list table
   ---@return table, table
   local function splitAt(index, list)
      local fst, lst = {}, {}
      for k, v in pairs(list) do
         if k <= index then
            fst[#fst + 1] = v
         else
            lst[#lst + 1] = v
         end
      end
      return fst, lst
   end
   ut.splitAt = splitAt
   
   ---@param step number
   ---@param list any[]
   ---@return table
   function ut.rotate(step, list)
      local a, b = splitAt(step, list)
      return concat(b, a)
   end
   
   ---pipe fuctions: pipe(f, g, h)(x) -> f(g(h(x)))
   ---@param fs (fun(x : any) : any)[]
   ---@return any
   function ut.pipe(fs)
      return reduce(function(f, g)
         return function(...)
            return f(g(...))
         end
      end, id, fs)
   end
   
   local function reverse(...)
      local function reverse_h(acc, v, ...)
         if 0 == select("#", ...) then
            return v, acc()
         else
            return reverse_h(function()
               return v, acc()
            end, ...)
         end
      end
      return reverse_h(function() end, ...)
   end
   
   ---curry given function -> f(a, b, c) -> f(a)(b)(c)
   ---@param func function
   ---@param num_args number
   ---@return function
   local function curry(func, num_args)
      num_args = num_args or 2
      if num_args <= 1 then
         return func
      end
      local function curry_h(argtrace, n)
         if 0 == n then
            return func(reverse(argtrace()))
         else
            return function(onearg)
               return curry_h(function()
                  return onearg, argtrace()
               end, n - 1)
            end
         end
      end
      return curry_h(function() end, num_args)
   end
   ut.curry = curry
   
   ---flip two args of f
   ---@param f function
   ---@return function
   function ut.flip(f)
      return function(a, b)
         return f(b, a)
      end
   end
   
   local function xorwise(x)
      local a = bxor(lshift(x, 13), x)
      local b = bxor(rshift(a, 17), a)
      return bxor(lshift(b, 5), b)
   end
   
   local function _frac(x)
      return (x - x:floor()):asFloat()
   end
   
   local function timeToIntSeed(x)
      return xorwise(floor((_frac(x / 300) * 536870912)))
   end
   
   local function intSeedToRand(x)
      return (x % 536870912) / 536870912
   end
   
   function ut.timeToRand(x)
      return abs(intSeedToRand(timeToIntSeed(x)))
   end
   
   local nparams
   ---returns num_param, is_vararg
   ---@param func function
   ---@return number, boolean
   function nparams(func)
      local info = d_getinfo(func)
      return info.nparams, info.isvararg
   end
   if _VERSION == "Lua 5.1" and not jit then
      function nparams(func)
         local s = str_dump(func)
         assert(s:sub(1, 6) == "\27LuaQ\0", "This code works only in Lua 5.1")
         local int_size = s:byte(8)
         local ptr_size = s:byte(9)
         local pos = 14 + ptr_size + (s:byte(7) > 0 and s:byte(13) or s:byte(12 + ptr_size)) + 2 * int_size
         return s:byte(pos), s:byte(pos + 1) > 0
      end
   end
   ut.nparams = nparams
   
   ---register a f(..., pat) as a method for Pattern.f(self, ...), essentially switch the order of args
   ---@param f function
   ---@return function
   function ut.method_wrap(f)
      return function(...)
         local args = { ... }
         local pat = tremove(args, 1)
         args[#args + 1] = pat
         return f(unpack(args))
      end
   end
   
   ---if f gets less args then arity, then curry the f and pass the current amount of args into it
   ---@param arity number
   ---@param f function
   ---@return function
   function ut.curry_wrap(arity, f)
      return function(...)
         local args = { ... }
         if #args < arity then
            local cf = curry(f, arity)
            for _, v in ipairs(args) do
               cf = cf(v)
            end
            return cf
         else
            return f(...)
         end
      end
   end
   
   ---for lua5.1 compatibility
   ---@param f any
   ---@param env any
   ---@return any
   function ut.setfenv(f, env)
      local i = 1
      while true do
         local name = d_getupvalue(f, i)
         if name == "_ENV" then
            d_setupvalue(f, i, env)
            break
         elseif not name then
            break
         end
         i = i + 1
      end
      return f
   end
   
   local function partition(array, left, right, pivotIndex)
      local pivotValue = array[pivotIndex]
      array[pivotIndex], array[right] = array[right], array[pivotIndex]
   
      local storeIndex = left
   
      for i = left, right - 1 do
         if array[i] <= pivotValue then
            array[i], array[storeIndex] = array[storeIndex], array[i]
            storeIndex = storeIndex + 1
         end
         array[storeIndex], array[right] = array[right], array[storeIndex]
      end
   
      return storeIndex
   end
   
   local function quicksort(array, left, right)
      if right > left then
         local pivotNewIndex = partition(array, left, right, left)
         quicksort(array, left, pivotNewIndex - 1)
         quicksort(array, pivotNewIndex + 1, right)
      end
   end
   ut.quicksort = quicksort
   
   function ut.get_args(f)
      local args = {}
      for i = 1, nparams(f) do
         args[#args + 1] = d_getlocal(f, i)
      end
      return args
   end
   if _VERSION == "Lua 5.1" and not jit then
      ut.get_args = function(f)
         local args = {}
         local hook = d_gethook()
   
         local argHook = function()
            local info = d_getinfo(3)
            if "pcall" ~= info.name then
               return
            end
   
            for i = 1, huge do
               local name = d_getlocal(2, i)
               if "(*temporary)" == name then
                  d_sethook(hook)
                  error ""
                  return
               end
               args[#args + 1] = name
            end
         end
   
         d_sethook(argHook, "c")
         pcall(f)
   
         return args
      end
   end
   
   function ut.getlocal(name, level)
      local value
      local found = false
   
      level = (level or 1) + 1
   
      for i = 1, huge do
         local n, v = d_getlocal(level, i)
         if not n then
            break
         end
         if n == name then
            value = v
            found = true
         end
      end
      if found then
         return value
      end
      -- try non-local variables
      local func = debug.getinfo(level, "f").func
      for i = 1, math.huge do
         local n, v = debug.getupvalue(func, i)
         if not n then
            break
         end
         if n == name then
            return v
         end
      end
   end
   
   ---@class switch
   ---@field cachedCases string[]
   ---@field map table<string, function>
   ---@field _default fun(...):...
   local switchMT = {}
   switchMT.__index = switchMT
   
   ---@param name string
   ---@return switch
   function switchMT:case(name)
      self.cachedCases[#self.cachedCases + 1] = name
      return self
   end
   
   ---@param callback async fun(...):...
   ---@return switch
   function switchMT:call(callback)
      for i = 1, #self.cachedCases do
         local name = self.cachedCases[i]
         self.cachedCases[i] = nil
         if self.map[name] then
            error("Repeated fields:" .. tostring(name))
         end
         self.map[name] = callback
      end
      return self
   end
   
   ---@param callback fun(...):...
   ---@return switch
   function switchMT:default(callback)
      self._default = callback
      return self
   end
   
   function switchMT:getMap()
      return self.map
   end
   
   ---@param name string
   ---@return boolean
   function switchMT:has(name)
      return self.map[name] ~= nil
   end
   
   ---@param name string
   ---@param ... any
   ---@return ...
   function switchMT:__call(name, ...)
      local callback = self.map[name] or self._default
      if not callback then
         return
      end
      return callback(...)
   end
   
   ---@return switch
   function ut.switch()
      local obj = setmetatable({
         map = {},
         cachedCases = {},
      }, switchMT)
      return obj
   end
   
end

do
   
   local T = ut.T
   local union = ut.union
   local abs = math.abs
   local floor = math.floor
   local setmetatable = setmetatable
   local tremove = table.remove
   local tconcat = table.concat
   local unpack = _G.unpack or table.unpack
   local is_array = ut.is_array
   
   local Time, Span, Event
   local time = { __class = "time" }
   local span = { __class = "span" }
   local event = { __class = "event" }
   span.__index = span
   event.__index = event
   time.__index = time
   
   function span:spanCycles()
      local spans = {}
      local b, e = self.start, self.stop
      local e_sam = e:sam()
      -- TODO: zero width???
      -- if b == e then
      --    return { Span(b, e) }
      -- end
      while e > b do
         if b:sam() == e_sam then
            spans[#spans + 1] = Span(b, self.stop)
            break
         end
         local next_b = b:nextSam()
         spans[#spans + 1] = Span(b, next_b)
         b = next_b
      end
      return spans
   end
   
   function span:duration()
      return self.stop - self.start
   end
   
   function span:midpoint()
      return self.start + (self:duration() / 2)
   end
   
   function span:cycleSpan()
      local b = self.start:cyclePos()
      return Span(b, b + self:duration())
   end
   
   function span:__eq(rhs)
      return self.start == rhs.start and self.stop == rhs.stop
   end
   
   function span:__tostring()
      return self.start:show() .. " → " .. self.stop:show()
   end
   
   function span:show()
      return self:__tostring()
   end
   
   function span:withTime(func)
      return Span(func(self.start), func(self.stop))
   end
   
   function span:withEnd(func)
      return Span(self.start, func(self.stop))
   end
   
   function span:withCycle(func)
      local sam = self.start:sam()
      local b = sam + func(self.start - sam)
      local e = sam + func(self.stop - sam)
      return Span(b, e)
   end
   
   function span:sect(other)
      local maxOfStart = self.start:max(other.start)
      local minOfEnd = self.stop:min(other.stop)
      if maxOfStart > minOfEnd then
         return nil
      end
      if maxOfStart == minOfEnd then
         if maxOfStart == self.stop and self.start < self.stop then
            return nil
         end
         if maxOfStart == other.stop and other.start < other.stop then
            return nil
         end
      end
      return Span(maxOfStart, minOfEnd)
   end
   
   function span:sect_e(other)
      local result = self:sect(other)
      if not result then
         error "Span: spans do not intersect"
      end
      return result
   end
   
   function Span(b, e)
      b = b or 1
      e = e or 1
      return setmetatable({
         start = Time(b),
         stop = Time(e),
      }, span)
   end
   
   function event:__eq(other)
      --    return (self.part == other.part)
      --       and (self.whole == other.whole)
      --       and (compare(self.value, other.value))
      --       and (compare(self.context, other.context))
      --       and (self.stateful == other.stateful)
      return self:__tostring() == other:__tostring()
   end
   
   function event:duration()
      return self.whole.stop - self.whole.start
   end
   
   function event:wholeOrPart()
      if self.whole ~= nil then
         return self.whole
      end
      return self.part
   end
   
   function event:hasWhole()
      return self.whole ~= nil
   end
   
   function event:hasOnset()
      return self.whole ~= nil and self.whole.start == self.part.start
   end
   
   function event:withSpan(func)
      local whole = self.whole
      if whole ~= nil then
         whole = func(whole)
      end
      return Event(whole, func(self.part), self.value, self.context, self.stateful)
   end
   
   function event:withValue(func)
      return Event(self.whole, self.part, func(self.value), self.context, self.stateful)
   end
   
   function event:show()
      return self:__tostring()
   end
   
   function event:__tostring()
      local part = self.part:__tostring()
      local h, t = "", ""
      if self:hasWhole() then
         h = (self.whole.start ~= self.part.start) and self.whole.start:show() .. "-" or ""
         t = (self.whole.stop ~= self.part.stop) and "-" .. self.whole.stop:show() or ""
      end
      return ("%s(%s)%s | %s"):format(h, part, t, ut.tdump(self.value))
   end
   
   function event:spanEquals(other)
      return ((other.whole == nil) and (self.whole == nil)) or (other.whole == self.whole)
   end
   
   function event:setContext(newContext)
      return Event(self.whole, self.part, self.value, newContext, self.stateful)
   end
   
   function event:combineContext(other)
      local newContext = {}
      for key, value in pairs(self.context) do
         newContext[key] = value
      end
      for key, value in pairs(other.context) do
         newContext[key] = value
      end
      local loc1 = self.context.locations or {}
      local loc2 = other.context.locations or {}
      for i = 1, #loc2 do
         loc1[#loc1 + 1] = loc2[i]
      end
      newContext.locations = loc1
      return newContext
   end
   
   function Event(whole, part, value, context, stateful)
      part = part or Span()
      context = context or {}
      stateful = stateful or false
      if stateful and T(value) ~= "function" then
         error "Event: stateful event values must be of type function"
      end
      return setmetatable({
         whole = whole,
         part = part,
         value = value,
         context = context,
         stateful = stateful,
      }, event)
   end
   
   local function decimaltofraction(x0, err)
      err = err or 0.0000000001
      local num, den
      local g = abs(x0)
      local sign = x0 / g
      local a, b, c, d = 0, 1, 1, 0
      local s
      local iter = 0
      while iter < 1000000 do
         s = floor(g)
         num = a + s * c
         den = b + s * d
         a, b, c, d = c, d, num, den
         g = 1.0 / (g - s)
         iter = iter + 1
         if err > abs(sign * num / den - x0) then
            return sign * num, den
         end
      end
      error("Time: failed to find a fraction for " .. x0)
      return 0, 1
   end
   
   local function gcd(a, b)
      return (b == 0) and a or gcd(b, a % b)
   end
   
   local function lcm(a, b)
      return (a == 0 or b == 0) and 0 or abs(a * b) / gcd(a, b)
   end
   
   function time:wholeCycle()
      return Span(self:sam(), self:nextSam())
   end
   
   function time:cyclePos()
      return self - self:sam()
   end
   
   function time:__add(f2)
      f2 = Time(f2)
      local na = self.numerator
      local nb = f2.numerator
      local da = self.denominator
      local db = f2.denominator
      local g = gcd(da, db)
      if g == 1 then
         Time(na * db + da * nb, da * db, false)
      end
      local s = floor(da / g)
      local t = na * floor(db / g) + nb * s
      local g2 = gcd(t, g)
      if g2 == 1 then
         Time(t, s * db, false)
      end
      return Time(floor(t / g2), s * floor(db / g2), false)
   end
   
   function time:__sub(f2)
      f2 = Time(f2)
      local na = self.numerator
      local nb = f2.numerator
      local da = self.denominator
      local db = f2.denominator
      local g = gcd(da, db)
      if g == 1 then
         Time(na * db - da * nb, da * db, false)
      end
      local s = floor(da / g)
      local t = na * floor(db / g) - nb * s
      local g2 = gcd(t, g)
      if g2 == 1 then
         Time(t, s * db, false)
      end
      return Time(floor(t / g2), s * floor(db / g2), false)
   end
   
   function time:__div(f2)
      f2 = Time(f2)
      local na = self.numerator
      local nb = f2.numerator
      local da = self.denominator
      local db = f2.denominator
      local g1 = gcd(na, nb)
      if g1 > 1 then
         na = floor(na / g1)
         nb = floor(nb / g1)
      end
      local g2 = gcd(db, da)
      if g2 > 1 then
         da = floor(da / g2)
         db = floor(db / g2)
      end
      local n = na * db
      local d = nb * da
      if d < 0 then
         n = -n
         d = -d
      end
      return Time(n, d, false)
   end
   
   function time:__mul(f2)
      f2 = Time(f2)
      local na = self.numerator
      local nb = f2.numerator
      local da = self.denominator
      local db = f2.denominator
      local g1 = gcd(na, db)
      if g1 > 1 then
         na = floor(na / g1)
         db = floor(db / g1)
      end
      local g2 = gcd(nb, da)
      if g2 > 1 then
         nb = floor(nb / g2)
         da = floor(da / g2)
      end
      return Time(na * nb, da * db, false)
   end
   
   function time:__pow(f2)
      f2 = Time(f2)
      if f2.denominator == 1 then
         local power = f2.numerator
         if power >= 0 then
            return Time(self.numerator ^ power, self.denominator ^ power, false)
         elseif self.numerator >= 0 then
            return Time(self.denominator ^ -power, self.numerator ^ -power, false)
         else
            return Time((-self.numerator) ^ -power, (-self.denominator) ^ -power, false)
         end
      else
         return (self.numerator / self.denominator) ^ (f2.numerator / f2.denominator)
      end
   end
   
   function time:__mod(f2)
      f2 = Time(f2)
      local da = self.denominator
      local db = f2.denominator
      local na = self.numerator
      local nb = f2.numerator
      return Time((na * db) % (nb * da), da * db)
   end
   
   function time:__unm()
      return Time(-self.numerator, self.denominator, false)
   end
   
   function time:__eq(rhs)
      return self.numerator / self.denominator == rhs.numerator / rhs.denominator
   end
   
   function time:__lt(rhs)
      return self.numerator / self.denominator < rhs.numerator / rhs.denominator
   end
   
   function time:__lte(rhs)
      return self.numerator / self.denominator <= rhs.numerator / rhs.denominator
   end
   
   function time:eq(rhs)
      return self == (Time(rhs))
   end
   
   function time:lt(rhs)
      return self < Time(rhs)
   end
   
   function time:gt(rhs)
      return self > Time(rhs)
   end
   
   function time:lte(rhs)
      return self <= Time(rhs)
   end
   
   function time:gte(rhs)
      return self <= Time(rhs)
   end
   
   function time:reverse()
      return Time(1) / self
   end
   
   function time:floor()
      return floor(self.numerator / self.denominator)
   end
   
   function time:sam()
      return Time(self:floor())
   end
   
   function time:nextSam()
      return self:sam() + 1
   end
   
   function time:min(other)
      other = Time(other)
      if self < other then
         return self
      else
         return other
      end
   end
   
   function time:max(other)
      other = Time(other)
      if self > other then
         return self
      else
         return other
      end
   end
   
   function time:gcd(other)
      other = Time(other)
      local gcd_numerator = gcd(self.numerator, other.numerator)
      local lcm_denominator = lcm(self.denominator, other.denominator)
      return Time(gcd_numerator, lcm_denominator)
   end
   
   function time:asFloat()
      return self.numerator / self.denominator
   end
   
   function time:__tostring()
      return ("%d/%d"):format(self.numerator, self.denominator)
   end
   
   function time:show()
      return self:__tostring()
   end
   
   ---@class Fraction
   function Time(n, d, normalize)
      -- HACK:
      if T(n) == "time" then
         return n
      end
      n = n or 0
      d = d or 1
      if normalize == nil then
         normalize = true
      end
      if n % 1 ~= 0 then
         n, d = decimaltofraction(n)
      end
      if d == 0 then
         error "Time: divide by zero"
      end
      if normalize and (n ~= 0) then
         local g = floor(gcd(n, d))
         n = floor(n / g)
         d = floor(d / g)
      end
      return setmetatable({
         numerator = n,
         denominator = d,
      }, time)
   end
   
   local stream = { __class = "stream" }
   
   function stream:notifyTick(cycleFrom, cycleTo, s, cps, bpc, mill, now)
      if not self.pattern then
         return
      end
      local events = self.pattern:onsetsOnly()(cycleFrom, cycleTo)
      for _, ev in ipairs(events) do
         local cycleOn = ev.whole.start
         local cycleOff = ev.whole.stop
         local linkOn = s:time_at_beat(cycleOn:asFloat() * bpc, 0)
         local linkOff = s:time_at_beat(cycleOff:asFloat() * bpc, 0)
         local deltaSeconds = (linkOff - linkOn) / mill
         local value = ev.value
         value.cps = ev.value.cps or cps
         value.cycle = cycleOn:asFloat()
         value.delta = deltaSeconds
         local link_secs = now / mill
         local nudge = 0
         local diff = losc:now() + -link_secs
         -- print(link_secs)
         -- print(diff:seconds())
         local ts = diff + (linkOn / mill) + self.latency + nudge
         self.callback(value, ts)
      end
   end
   stream.__index = stream
   
   local function Stream(callback)
      return setmetatable({ latency = 0.2, callback = callback }, stream)
   end
   
   local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct
   
   local function pId(...)
      return { tconcat { ... } }
   end
   
   local function pComp(const, tvar)
      return { constructor = const[1], tvar[1] }
   end
   
   local function pDef(...)
      local args = { ... }
      local name
      if args[1].isname then
         name = tremove(args, 1)[1]
      end
      local ret = tremove(args, #args)
      return { ret = ret, name = name, unpack(args) }
   end
   
   local function pTab(a)
      a.istable = true
      return a
   end
   
   local typedef = V "typedef"
   local fdef = V "fdef"
   local tab = V "tab"
   local elem = V "elem"
   local comp_type = V "comp_type"
   local char = R("AZ", "az")
   local name = V "name"
   local ws = S " \n\r\t" ^ 0
   local id = ws * ((char ^ 1) / pId) * ws
   
   local rules = {
      [1] = "typedef",
      name = id * ws * P "::" * ws / function(a)
         a.isname = true
         return a
      end,
      typedef = name ^ -1 * (elem * ws * P "->" * ws) ^ 1 * elem / pDef,
      elem = comp_type + id + fdef + tab,
      fdef = P "(" * ws * typedef * ws * P ")",
      tab = P "[" * ws * elem * ws * P "]" / pTab,
      comp_type = id * ws * id / pComp,
   }
   
   local grammar = Ct(C(rules))
   
   local function TDef(a)
      local tdef = grammar:match(a)[2]
      tdef.source = a
      setmetatable(tdef, {
         __tostring = function(self)
            return self.source
         end,
      })
      return tdef, tdef.name
   end
   
   local valuemap = {
      -- TODO: cover if other types
      __add = function(t1, t2)
         if type(t2) == "number" then
            local k, v = next(t1)
            return { [k] = v + t2 }
         elseif type(t2) == "table" and not is_array(t2) then
            for k, v in pairs(t1) do
               if type(v) == "number" or tonumber(v) then
                  t1[k] = v + (t2[k] or 0)
               end
            end
            for k, v in pairs(t2) do
               if not t1[k] then
                  t1[k] = v
               end
            end
            return t1
         else
            error "bad table arith"
         end
      end,
      __sub = function(t1, t2)
         if type(t2) == "number" then
            local k, v = next(t1)
            return { [k] = v - t2 }
         elseif type(t2) == "table" and not is_array(t2) then
            for k, v in pairs(t1) do
               if type(v) == "number" or tonumber(v) then
                  t1[k] = v - (t2[k] or 0)
               end
            end
            for k, v in pairs(t2) do
               if not t1[k] then
                  t1[k] = v
               end
            end
            return t1
         else
            error "bad table arith"
         end
      end,
      __unm = function(t)
         local k, v = next(t)
         -- TODO: check
         return { [k] = -v }
      end,
      __concat = function(lhs, rhs)
         return union(lhs, rhs)
      end,
   }
   valuemap.__index = valuemap
   
   local function ValueMap(valmap)
      return setmetatable(valmap, valuemap)
   end
   
   types = { Span = Span, Event = Event, Time = Time, Stream = Stream, TDef = TDef, ValueMap = ValueMap }
   
end

do
   -- Copyright (c) 2006-2013 Fabien Fleutot and others.
   a2s.__index = a2s
   
   local tconcat = table.concat
   local str_match = string.match
   local str_format = string.format
   local unpack = unpack or rawget(table, "unpack")
   
   -- TODO: check AST
   
   -- Instanciate a new AST->source synthetizer
   function a2s.new()
      local self = {
         _acc = {}, -- Accumulates pieces of source as strings
         current_indent = 0, -- Current level of line indentation
         indent_step = "   ", -- Indentation symbol, normally spaces or '\t'
      }
      return setmetatable(self, a2s)
   end
   
   --------------------------------------------------------------------------------
   -- Run a synthetizer on the `ast' arg and return the source as a string.
   -- Can also be used as a static method `M.run (ast)'; in this case,
   -- a temporary Metizer is instanciated on the fly.
   --------------------------------------------------------------------------------
   function a2s:run(ast)
      if not ast then
         self, ast = a2s.new(), self
      end
      self._acc = {}
      self:node(ast)
      return tconcat(self._acc)
   end
   
   --------------------------------------------------------------------------------
   -- Accumulate a piece of source file in the synthetizer.
   --------------------------------------------------------------------------------
   function a2s:acc(x)
      if x then
         self._acc[#self._acc + 1] = x
      end
   end
   
   --------------------------------------------------------------------------------
   -- Accumulate an indented newline.
   -- Jumps an extra line if indentation is 0, so that
   -- toplevel definitions are separated by an extra empty line.
   --------------------------------------------------------------------------------
   function a2s:nl()
      if self.current_indent == 0 then
         self:acc "\n"
      end
      self:acc("\n" .. self.indent_step:rep(self.current_indent))
   end
   
   --------------------------------------------------------------------------------
   -- Increase indentation and accumulate a new line.
   --------------------------------------------------------------------------------
   function a2s:nlindent()
      self.current_indent = self.current_indent + 1
      self:nl()
   end
   
   --------------------------------------------------------------------------------
   -- Decrease indentation and accumulate a new line.
   --------------------------------------------------------------------------------
   function a2s:nldedent()
      self.current_indent = self.current_indent - 1
      self:acc("\n" .. self.indent_step:rep(self.current_indent))
   end
   
   --------------------------------------------------------------------------------
   -- Keywords, which are illegal as identifiers.
   --------------------------------------------------------------------------------
   local keywords_list = {
      "and",
      "break",
      "do",
      "else",
      "elseif",
      "end",
      "false",
      "for",
      "function",
      "if",
      "in",
      "local",
      "nil",
      "not",
      "or",
      "repeat",
      "return",
      "then",
      "true",
      "until",
      "while",
   }
   local keywords = {}
   for _, kw in pairs(keywords_list) do
      keywords[kw] = true
   end
   
   --------------------------------------------------------------------------------
   -- Return true iff string `id' is a legal identifier name.
   --------------------------------------------------------------------------------
   local function is_ident(id)
      return str_match(id, "^[%a_][%w_]*$") and not keywords[id]
   end
   
   -- Return true iff ast represents a legal function name for
   -- syntax sugar ``function foo.bar.gnat() ... end'':
   -- a series of nested string indexes, with an identifier as
   -- the innermost node.
   local function is_idx_stack(ast)
      local tag = ast.tag
      if tag == "Index" then
         return is_idx_stack(ast[1])
      elseif tag == "Id" then
         return true
      else
         return false
      end
   end
   
   --------------------------------------------------------------------------------
   -- Operator precedences, in increasing order.
   -- This is not directly used, it's used to generate op_prec below.
   --------------------------------------------------------------------------------
   local op_preprec = {
      { "or", "and" },
      { "lt", "le", "eq", "ne" },
      { "concat" },
      { "add", "sub" },
      { "mul", "div", "mod" },
      { "unm", "unary", "not", "len" }, ---TODO:
      { "pow" },
      { "index" },
   }
   
   --------------------------------------------------------------------------------
   -- operator --> precedence table, generated from op_preprec.
   --------------------------------------------------------------------------------
   local op_prec = {}
   
   for prec, ops in ipairs(op_preprec) do
      for _, op in ipairs(ops) do
         op_prec[op] = prec
      end
   end
   
   --------------------------------------------------------------------------------
   -- operator --> source representation.
   --------------------------------------------------------------------------------
   local op_symbol = {
      add = " + ",
      sub = " - ",
      mul = " * ",
      div = " / ",
      mod = " % ",
      pow = " ^ ",
      concat = " .. ",
      eq = " == ",
      ne = " ~= ",
      lt = " < ",
      le = " <= ",
      ["and"] = " and ",
      ["or"] = " or ",
      ["not"] = "not ",
      len = "# ",
      unm = "-",
   }
   -- Accumulate the source representation of AST `node' in
   -- the synthetizer. Most of the work is done by delegating to
   -- the method having the name of the AST tag.
   -- If something can't be converted to normal sources, it's
   -- instead dumped as a `-{ ... }' splice in the source accumulator.
   function a2s:node(node)
      assert(self ~= a2s and self._acc, "wrong ast_to_src compiler?")
      if node == nil then
         self:acc "<<error>>"
         return
      end
      if not node.tag then -- tagless block.
         self:list(node, self.nl)
      else
         local f = a2s[node.tag]
         if type(f) == "function" then -- Delegate to tag method.
            f(self, node, unpack(node))
         elseif type(f) == "string" then -- tag string.
            self:acc(f)
         end
      end
   end
   
   --------------------------------------------------------------------------------
   -- Convert every node in the AST list `list' passed as 1st arg.
   -- `sep' is an optional separator to be accumulated between each list element,
   -- it can be a string or a synth method.
   -- `start' is an optional number (default == 1), indicating which is the
   -- first element of list to be converted, so that we can skip the begining
   -- of a list.
   --------------------------------------------------------------------------------
   function a2s:list(list, sep, start)
      for i = start or 1, #list do
         self:node(list[i])
         if list[i + 1] then
            if not sep then
               return -- HACK:
            elseif type(sep) == "function" then
               sep(self)
            elseif type(sep) == "string" then
               self:acc(sep)
            else
               error "Invalid list separator"
            end
         end
      end
   end
   
   --------------------------------------------------------------------------------
   --
   -- Tag methods.
   -- ------------
   --
   -- Specific AST node dumping methods, associated to their node kinds
   -- by their name, which is the corresponding AST tag.
   -- synth:node() is in charge of delegating a node's treatment to the
   -- appropriate tag method.
   --
   -- Such tag methods are called with the AST node as 1st arg.
   -- As a convenience, the n node's children are passed as args #2 ... n+1.
   --
   -- There are several things that could be refactored into common subroutines
   -- here: statement blocks dumping, function dumping...
   -- However, given their small size and linear execution
   -- (they basically perform series of :acc(), :node(), :list(),
   -- :nl(), :nlindent() and :nldedent() calls), it seems more readable
   -- to avoid multiplication of such tiny functions.
   --
   -- To make sense out of these, you need to know metalua's AST syntax, as
   -- found in the reference manual or in metalua/doc/ast.txt.
   --
   --------------------------------------------------------------------------------
   
   function a2s:Chunk(node)
      -- TODO: check ret last
      for _, v in ipairs(node) do
         self:node(v)
         self:acc "; "
      end
   end
   
   function a2s:Do(node)
      self:acc "do"
      self:nlindent()
      self:list(node, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Set(node)
      local lhs = node[1]
      local rhs = node[2]
      -- ``function foo:bar(...) ... end'' --
      if
         lhs[1].tag == "Index"
         and rhs[1].tag == "Function"
         and rhs[1][1][1] == "self"
         and is_idx_stack(lhs)
         and is_ident(lhs[1][2][1])
      then
         local method = lhs[1][2][1]
         local params = rhs[1][1]
         local body = rhs[1][2]
         self:acc "function "
         self:node(lhs)
         self:acc ":"
         self:acc(method)
         self:acc "("
         self:list(params, ", ", 2)
         self:acc ")"
         self:nlindent()
         self:list(body, self.nl)
         self:nldedent()
         self:acc "end"
      elseif rhs[1].tag == "Function" and is_idx_stack(lhs) then
         -- | `Set{ { lhs }, { `Function{ params, body } } } if is_idx_stack (lhs) ->
         -- ``function foo(...) ... end'' --
         local params = rhs[1][1]
         local body = rhs[1][2]
         self:acc "function "
         self:node(lhs)
         self:acc "("
         self:list(params, ", ")
         self:acc ")"
         self:nlindent()
         self:list(body, self.nl)
         self:nldedent()
         self:acc "end"
      else
         self:list(lhs, ", ")
         self:acc " = "
         self:list(rhs, ", ")
      end
   end
   
   function a2s:While(_, cond, body)
      self:acc "while "
      self:node(cond)
      self:acc " do"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Repeat(_, body, cond)
      self:acc "repeat"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "until "
      self:node(cond)
   end
   
   function a2s:If(node)
      for i = 1, #node - 1, 2 do
         -- for each ``if/then'' and ``elseif/then'' pair --
         local cond, body = node[i], node[i + 1]
         self:acc(i == 1 and "if " or "elseif ")
         self:node(cond)
         self:acc " then"
         self:nlindent()
         self:list(body, self.nl)
         self:nldedent()
      end
      -- odd number of children --> last one is an `else' clause --
      if #node % 2 == 1 then
         self:acc "else"
         self:nlindent()
         self:list(node[#node], self.nl)
         self:nldedent()
      end
      self:acc "end"
   end
   
   function a2s:Fornum(node, var, first, last)
      local body = node[#node]
      self:acc "for "
      self:node(var)
      self:acc " = "
      self:node(first)
      self:acc ", "
      self:node(last)
      if #node == 5 then -- 5 children --> child #4 is a step increment.
         self:acc ", "
         self:node(node[4])
      end
      self:acc " do"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Forin(_, vars, generators, body)
      self:acc "for "
      self:list(vars, ", ")
      self:acc " in "
      self:list(generators, ", ")
      self:acc " do"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Local(_, lhs, rhs, annots)
      self:acc "local "
      if annots then
         local n = #lhs
         for i = 1, n do
            self:node(lhs)
            local a = annots[i]
            if a then
               self:acc " #"
               self:node(a)
            end
            if i ~= n then
               self:acc ", "
            end
         end
      else
         self:list(lhs, ", ")
      end
      if rhs[1] then
         self:acc " = "
         self:list(rhs, ", ")
      end
   end
   
   function a2s:Localrec(_, lhs, rhs)
      -- ``local function name() ... end'' --
      self:acc "local function "
      self:acc(lhs[1][1])
      self:acc "("
      self:list(rhs[1][1], ", ")
      self:acc ")"
      self:nlindent()
      self:list(rhs[1][2], self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Call(node, f)
      local parens
      if node[2].tag == "String" or node[2].tag == "Table" then
         parens = false
      else
         parens = true
      end
      self:node(f)
      self:acc(parens and "(" or " ")
      self:list(node, ", ", 2) -- skip `f'.
      self:acc(parens and ")")
   end
   
   function a2s:Invoke(node, f, method)
      -- single string or table literal arg ==> no need for parentheses. --
      local parens
      if node[2].tag == "String" or node[2].tag == "Table" then
         parens = false
      else
         parens = true
      end
      self:node(f)
      self:acc ":"
      self:acc(method[1])
      self:acc(parens and "(" or " ")
      self:list(node, ", ", 3) -- Skip args #1 and #2, object and method name.
      self:acc(parens and ")")
   end
   
   function a2s:Return(node)
      self:acc "return "
      self:list(node, ", ")
   end
   
   a2s.Break = "break"
   a2s.Nil = "nil"
   a2s.False = "false"
   a2s.True = "true"
   a2s.Dots = "..."
   
   function a2s:Number(_, n)
      self:acc(tostring(n))
   end
   
   function a2s:String(_, str)
      -- format "%q" prints '\n' in an umpractical way IMO,
      -- so this is fixed with the :gsub( ) call.
      self:acc(str_format("%q", str):gsub("\\\n", "\\n"))
   end
   
   function a2s:Function(_, params, body, annots)
      self:acc "function("
      if annots then
         local n = #params
         for i = 1, n do
            local p, a = params[i], annots[i]
            self:node(p)
            if annots then
               self:acc " #"
               self:node(a)
            end
            if i ~= n then
               self:acc ", "
            end
         end
      else
         self:list(params, ", ")
      end
      self:acc ")"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Table(node)
      if not node[1] then
         self:acc "{ }"
      else
         self:acc "{ "
         for i, elem in ipairs(node) do
            if elem.tag == "Pair" then
               -- `Pair{ `String{ key }, value }
               if elem[1].tag == "String" and is_ident(elem[1][1]) then
                  self:acc(elem[1][1])
                  self:acc " = "
                  self:node(elem[2])
               else
                  self:acc "["
                  self:node(elem[1])
                  self:acc "] = "
                  self:node(elem[2])
               end
            else
               self:node(elem)
            end
            if node[i + 1] then
               self:acc ", "
            end
         end
         self:acc " }"
      end
   end
   
   -- TODO: understand associatitivity
   function a2s:Op(node, op, a, b)
      if op == "not" and (node[2][1][1] == "eq") then ---TODO:???
         op, a, b = "ne", node[2][1][2], node[2][1][3]
      end
      if b then -- binary operator.
         local left_paren, right_paren
         if a.tag == "Op" and op_prec[op] >= op_prec[a[1]] then
            left_paren = true
         else
            left_paren = false
         end
         if b.tag == "Op" and op_prec[op] >= op_prec[b[1]] then
            right_paren = true
         else
            right_paren = false
         end
         self:acc(left_paren and "(")
         self:node(a)
         self:acc(left_paren and ")")
   
         self:acc(op_symbol[op])
   
         self:acc(right_paren and "(")
         self:node(b)
         self:acc(right_paren and ")")
      else -- unary operator.
         local paren
         if a.tag == "Op" and op_prec[op] >= op_prec[a[1]] then
            paren = true
         else
            paren = false
         end
         self:acc(op_symbol[op])
         self:acc(paren and "(")
         self:node(a)
         self:acc(paren and ")")
      end
   end
   
   function a2s:Paren(_, content)
      self:acc "("
      self:node(content)
      self:acc ")"
   end
   
   function a2s:Index(_, table, key)
      local paren_table
      if table.tag == "Op" and op_prec[table[1][1]] < op_prec.index then
         paren_table = true
      else
         paren_table = false
      end
   
      self:acc(paren_table and "(")
      self:node(table)
      self:acc(paren_table and ")")
   
      -- ``table [key]''
      if key.tag == "String" and is_ident(key[1]) then
         self:acc "."
         self:acc(key[1])
      else
         self:acc "["
         self:node(key)
         self:acc "]"
         -- ``table.key''
      end
   end
   
   function a2s:Id(_, name)
      if is_ident(name) then
         self:acc(name)
      else
         error "invalid identifier"
      end
   end
   
   function a2s:Goto(node, name)
      self:acc "goto "
      if type(name) == "string" then
         self:Id(node, name)
      else
         self:Id(node[1], node[1][1])
      end
   end
   
   function a2s:Label(node, name)
      self:acc "::"
      if type(name) == "string" then
         self:Id(node, name)
      else
         self:Id(node[1], node[1][1])
      end
      self:acc "::"
   end
   
end

do
   
   local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct
   
   
   local loadstring = ut.loadstring
   local setfenv = setfenv or ut.setfenv
   local memoize = ut.memoize
   local tremove = table.remove
   local ipairs = ipairs
   local type = type
   local filter = ut.filter
   local map = ut.map
   local reduce = ut.reduce
   local unpack = _G.unpack or rawget(table, "unpack")
   
   local sequence = V "sequence"
   local slice = V "slice"
   local sub_cycle = V "sub_cycle"
   local polymeter = V "polymeter"
   local slow_sequence = V "slow_sequence"
   local polymeter_steps = V "polymeter_steps"
   local stack = V "stack"
   local mini = V "mini"
   local op = V "op"
   local fast = V "fast"
   local slow = V "slow"
   local rand = V "rand"
   local replicate = V "replicate"
   local degrade = V "degrade"
   local weight = V "weight"
   local euclid = V "euclid"
   local tail = V "tail"
   local range = V "range"
   local list = V "list"
   local dollar = V "dollar"
   local tailop = V "tailop"
   local expr = V "expr"
   local ret = V "ret"
   local stat = V "stat"
   local choose = V "choose"
   local dotStack = V "dotStack"
   
   local function Id(a)
      return { tag = "Id", a }
   end
   
   local function Table(a)
      return { tag = "Table", unpack(a) }
   end
   
   local function Str(a)
      return { tag = "String", a }
   end
   
   local function Num(a)
      return { tag = "Number", a }
   end
   
   local function id(x)
      return x
   end
   
   local function Call(name, ...)
      return { tag = "Call", Id(name), ... }
   end
   
   local seed = -1 -- TODO: use this?
   local ws = S " \n\r\t" ^ 0
   local comma = ws * P "," * ws
   local pipe = ws * P "|" * ws
   local dot = ws * P "." * ws
   
   local function pNumber(num)
      return Num(tonumber(num))
   end
   
   local bool = { ["t"] = { tag = "True" }, ["f"] = { tag = "False" } }
   
   local function pStep(chars)
      if chars == "~" then
         return Id "silence"
      elseif tonumber(chars) then
         return Num(tonumber(chars))
      elseif bool[chars] then
         return bool[chars]
      elseif chars:sub(0, 1) == "'" then
         return Id(chars:sub(2, #chars))
      end
      return Str(chars)
   end
   
   local function rTails(args)
      local f = tremove(args, 1)
      if f.tag == "String" then
         f.tag = "Id"
      end
      local params = filter(function(a)
         return type(a) ~= "function"
      end, args)
      local tails = filter(function(a)
         return type(a) == "function"
      end, args)
      local main = { tag = "Call", f, unpack(params) }
      for i = 1, #tails do
         main = tails[i](main)
      end
      return main
   end
   
   local step_char = R("09", "AZ", "az") + S [[~^'._]]
   local tidalop = (S "|+-*/^%><" ^ 2 + P "#") / id
   local arith = (S "+-*/^%" - P "|") / id
   local step = ws * ((step_char ^ 1 - P ".") / pStep) * ws
   local minus = P "-"
   local plus = P "+"
   local zero = P "0"
   local digit = R "09"
   local decimal_point = P "."
   local digit1_9 = R "19"
   local e = S "eE"
   local int = zero + (digit1_9 * digit ^ 0)
   local exp = e * (minus + plus) ^ -1 * digit ^ 1
   local frac = decimal_point * digit ^ 1
   local number = (minus ^ -1 * int * frac ^ -1 * exp ^ -1) / pNumber
   
   local function pFast(a)
      return function(x)
         return Call("fast", a, x)
      end
   end
   
   local function pSlow(a)
      return function(x)
         return Call("slow", a, x)
      end
   end
   
   -- local function pRand(a)
   --    lower = a[1] or 0
   --    return function(x)
   --       -- TODO: idea rand run
   --       return Num(math.random(lower, x[1]))
   --    end
   -- end
   
   local function pDegrade(a)
      if a == "?" then
         a = Num(0.5)
      end
      return function(x)
         seed = seed + 1
         return Call("degradeBy", a, x)
      end
   end
   
   local function pTail(b)
      return function(a)
         return Call("chain", a, b)
      end
   end
   
   local function pEuclid(p, s, r)
      r = r or Num(0)
      return function(x)
         return Call("euclidRot", p, s, r, x)
      end
   end
   
   local function pRange(s)
      return function(x)
         x.range = s[1]
         x.reps = nil
         return x
      end
   end
   
   local function pWeight(a)
      return function(x)
         x.weight = (x.weight or 1) + (tonumber(a[1]) or 2) - 1
         return x
      end
   end
   
   local function pReplicate(a)
      return function(x)
         x.reps = (x.reps or 1) + (tonumber(a[1]) or 2) - 1
         return x
      end
   end
   
   local function rReps(ast)
      local res = {}
      for _, node in ipairs(ast) do
         if node.reps then
            local reps = node.reps
            for _ = 1, reps do
               node.reps = nil
               res[#res + 1] = node
            end
         elseif node.range then
            for i = node[1], node.range do
               res[#res + 1] = Num(i)
            end
         else
            res[#res + 1] = node
         end
      end
      return res
   end
   
   local function pSlices(sli, ...)
      for _, v in ipairs { ... } do
         sli = v(sli)
      end
      return sli
   end
   
   local function addWeight(a, b)
      b = b.weight and b.weight or 1
      return a + b
   end
   
   local function rWeight(args)
      local acc = {}
      for _, v in ipairs(args) do
         acc[#acc + 1] = v.weight and Num(v.weight) or Num(1)
         acc[#acc + 1] = v
      end
      return acc
   end
   
   local function pSeq(isSlow)
      return function(args)
         local weightSum = reduce(addWeight, 0, args)
         if weightSum > #args then
            return Call(isSlow and "arrange" or "timecat", Table(rWeight(args)))
         else
            if #args == 1 then
               if isSlow then
                  return Call("pure", args[1])
               end
               return args[1]
            end
            return Call(isSlow and "slowcat" or "fastcat", Table(args))
         end
      end
   end
   
   local function pStack(...)
      local args = map(rReps, { ... })
      return rReps(args), "Stack"
   end
   
   local function pChoose(...)
      local args = map(rReps, { ... })
      return rReps(args), "Choose"
   end
   
   local function pDotStack(...)
      local args = map(rReps, { ... })
      return rReps(args), "DotStack"
   end
   
   local opsymb = {
      ["+"] = "add",
      ["-"] = "sub",
      ["*"] = "mul",
      ["/"] = "div",
      ["^"] = "pow",
      ["%"] = "mod",
      -- ["+"] = { "add", true },
      -- ["-"] = { "sub", true },
      -- ["*"] = { "mul", true },
      -- ["/"] = { "div", true },
      -- ["^"] = { "pow", true },
      -- ["%"] = { "mod", true },
      -- ["."] = { "pipe", false },
   }
   
   local function is_op(a)
      return opsymb[a]
   end
   
   local function pDollar(...)
      local args = { ... }
      if #args == 1 then
         return args
      end
      return rTails(args)
   end
   
   local function pList(...)
      local args = { ... }
      if is_op(args[1]) then
         local opname = opsymb[args[1]]
         if #args == 3 then
            tremove(args, 1)
            return { tag = "Op", opname, unpack(args) }
         elseif #args == 2 then
            return {
               tag = "Paren",
               { tag = "Function", { Id "x" }, { { tag = "Return", { tag = "Op", opname, Id "x", args[2] } } } },
            }
         else
            return args[1]
         end
      end
      return rTails(args)
   end
   
   local function pTailop(...)
      local args = { ... }
      local symb = tremove(args, 1)
      args = pDollar(unpack(args))
      return function(x)
         return { tag = "Call", { tag = "Index", Id "op", Str(symb) }, x, args }
      end
   end
   
   local function pSubCycle(args, tag)
      args = map(pSeq(false), args)
      if tag == "Stack" then
         return Call("stack", Table(args))
      elseif tag == "Choose" then
         return Call("randcat", Table(args))
      elseif tag == "DotStack" then
         return Call("fastcat", Table(args))
      end
   end
   
   local function pPolymeterSteps(s)
      return (s ~= "") and s or -1
   end
   
   local function pPolymeter(args, _, steps)
      steps = (steps == -1) and Num(#args[1]) or steps
      args = map(pSeq(false), args)
      return Call("polymeter", steps, Table(args))
   end
   
   local function pSlowSeq(args, tag)
      if tag then
         args = map(pSeq(false), args)
         if tag == "DotStack" then
            return Call("slowcat", Table(args))
         elseif tag == "Choose" then
            return Call("randcat", Table(args))
         end
      end
      return pSeq(true)(rReps(args))
   end
   
   local function pRoot(...)
      local stats = { ... }
      for i, a in ipairs(stats) do
         stats[i] = a
      end
      ---@diagnostic disable-next-line: inject-field
      stats.tag = "Chunk"
      return stats
   end
   
   local function pRet(a)
      return { tag = "Return", a }
   end
   
   local function pSet(lhs, rhs)
      lhs.tag = "Id"
      return { tag = "Set", { lhs }, { rhs } }
   end
   
   local function pStat(...)
      if select("#", ...) == 1 then
         return pRet { ... }
      end
      if select(2, ...) == "=" then
         return pSet(select(1, ...), select(3, ...))
      end
      return pRet(rTails { ... })
   end
   
   local function pDot(...)
      return { ... }
   end
   local tab = V "tab"
   
   local function pTab(...)
      return Table { ... }
   end
   
   local semi = P ";" ^ -1
   local grammar = {
      [1] = "root",
      root = (ret * semi) ^ 1 / pRoot,
      ret = (list + mini + dollar) / pRet,
      list = ws * P "(" * ws * (expr + arith) * expr ^ 0 * ws * P ")" * ws / pList,
      tab = ws * P "'(" * ws * expr ^ 1 * ws * P ")" * ws / pTab,
      dollar = S "$>" * ws * step * ws * expr ^ 0 * ws / pDollar,
      expr = ws * (tab + mini + list + dollar + tailop) * ws,
      sequence = (mini ^ 1) / pDot,
      stack = sequence * (comma * sequence) ^ 0 / pStack,
      choose = sequence * (pipe * sequence) ^ 1 / pChoose,
      dotStack = sequence * (dot * sequence) ^ 1 / pDotStack,
      tailop = tidalop * ws * step * ws * mini * ws / pTailop,
      mini = (slice * op ^ 0) / pSlices,
      slice = step + number + sub_cycle + polymeter + slow_sequence + list,
      sub_cycle = P "[" * ws * (dotStack + choose + stack) * ws * P "]" / pSubCycle,
      slow_sequence = P "<" * ws * (dotStack + choose + sequence) * ws * P ">" / pSlowSeq,
      polymeter = P "{" * ws * stack * ws * P "}" * polymeter_steps * ws / pPolymeter,
      polymeter_steps = (P "%" * slice) ^ -1 / pPolymeterSteps,
      -- op = fast + slow + tail + range + replicate + degrade + weight + euclid + rand,
      op = fast + slow + tail + range + replicate + degrade + weight + euclid,
      fast = P "*" * slice / pFast,
      slow = P "/" * slice / pSlow,
      tail = P ":" * slice / pTail,
      range = P ".." * ws * slice / pRange,
      -- rand = P "#" * (number ^ -1) / pRand,
      degrade = P "?" * (number ^ -1) / pDegrade,
      replicate = ws * P "!" * (number ^ -1) / pReplicate,
      weight = ws * (P "@" + P "_") * (number ^ -1) / pWeight,
      euclid = P "(" * ws * mini * comma * mini * ws * comma ^ -1 * mini ^ -1 * ws * P ")" / pEuclid,
   }
   
   local function make_gen(top_level)
      if top_level then
         stat = expr * (P "=" / id) ^ -1 * expr ^ 0 * ws / pStat
         grammar.root = (stat * semi) ^ 1 / pRoot
      else
         grammar.root = (ret * semi) ^ 1 / pRoot
      end
   
      local rules = Ct(C(grammar))
   
      local function read(str)
         return rules:match(str)[2]
      end
   
      return function(env)
         local to_str = function(src)
            local ok, ast
            ok, ast = pcall(read, src)
            if not ok then
               return false
            end
            local lua_src = a2s.run(ast) -- TODO: imporve api
            return lua_src
         end
   
         local function to_f(src)
            if not top_level then
               src = "[" .. src .. "]"
            end
            local ok, fn
            local lua_src = to_str(src)
            -- print(lua_src)
            if not lua_src then
               return false
            end
            ok, fn = pcall(loadstring, lua_src)
            if not ok then
               return false
            end
            setfenv(fn and fn or function()
               print "not a valid maxi notation"
            end, env)
            return fn()
         end
         return memoize(to_f)
      end
   end
   
   notation = { maxi = make_gen(true), mini = make_gen(false) }
   
end

do
   local P, S, V, R, C, Ct, Cc = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct, lpeg.Cc
   
   local concat, map = ut.concat, ut.map
   local qsort = ut.quicksort
   
   -- TODO: handle error for wrong chord names ...
   ---@enum (key) Chords
   local chordTable = {
      major = { 0, 4, 7 },
      aug = { 0, 4, 8 },
      six = { 0, 4, 7, 9 },
      sixNine = { 0, 4, 7, 9, 14 },
      major7 = { 0, 4, 7, 11 },
      major9 = { 0, 4, 7, 11, 14 },
      add9 = { 0, 4, 7, 14 },
      major11 = { 0, 4, 7, 11, 14, 17 },
      add11 = { 0, 4, 7, 17 },
      major13 = { 0, 4, 7, 11, 14, 21 },
      add13 = { 0, 4, 7, 21 },
      dom7 = { 0, 4, 7, 10 },
      dom9 = { 0, 4, 7, 14 },
      dom11 = { 0, 4, 7, 17 },
      dom13 = { 0, 4, 7, 21 },
      sevenFlat5 = { 0, 4, 6, 10 },
      sevenSharp5 = { 0, 4, 8, 10 },
      sevenFlat9 = { 0, 4, 7, 10, 13 },
      nine = { 0, 4, 7, 10, 14 },
      eleven = { 0, 4, 7, 10, 14, 17 },
      thirteen = { 0, 4, 7, 10, 14, 17, 21 },
      minor = { 0, 3, 7 },
      diminished = { 0, 3, 6 },
      minorSharp5 = { 0, 3, 8 },
      minor6 = { 0, 3, 7, 9 },
      minorSixNine = { 0, 3, 9, 7, 14 },
      minor7flat5 = { 0, 3, 6, 10 },
      minor7 = { 0, 3, 7, 10 },
      minor7sharp5 = { 0, 3, 8, 10 },
      minor7flat9 = { 0, 3, 7, 10, 13 },
      minor7sharp9 = { 0, 3, 7, 10, 15 },
      diminished7 = { 0, 3, 6, 9 },
      minor9 = { 0, 3, 7, 10, 14 },
      minor11 = { 0, 3, 7, 10, 14, 17 },
      minor13 = { 0, 3, 7, 10, 14, 17, 21 },
      minorMajor7 = { 0, 3, 7, 11 },
      one = { 0 },
      five = { 0, 7 },
      sus2 = { 0, 2, 7 },
      sus4 = { 0, 5, 7 },
      sevenSus2 = { 0, 2, 7, 10 },
      sevenSus4 = { 0, 5, 7, 10 },
      nineSus4 = { 0, 5, 7, 10, 14 },
      sevenFlat10 = { 0, 4, 7, 10, 15 },
      nineSharp5 = { 0, 1, 13 },
      minor9sharp5 = { 0, 1, 14 },
      sevenSharp5flat9 = { 0, 4, 8, 10, 13 },
      minor7sharp5flat9 = { 0, 3, 8, 10, 13 },
      elevenSharp = { 0, 4, 7, 10, 14, 18 },
      minor11sharp = { 0, 3, 7, 10, 14, 18 },
   }
   
   local alias = {
      major = { "maj", "M" },
      minor = { "min", "m" },
      aug = { "plus", "sharp5" },
      diminished = "dim",
      diminished7 = "dim7",
      one = "1",
      five = "5",
      six = "6",
      nine = "9", -- ?????
      eleven = "11",
      thirteen = "13",
   
      major7 = "maj7",
      major9 = "maj9",
      major11 = "maj11",
      major13 = "maj13",
   
      minor7 = { "min7", "m7" },
      minor9 = { "min9", "m9" },
      minor11 = { "min11", "m11" },
      minor13 = { "min13", "m13" },
   
      sixNine = { "six9", "sixby9", "6by9" },
   
      sevenFlat5 = "7f5",
      sevenSharp5 = "7s5",
      sevenFlat9 = "7f9",
      minorSharp5 = { "msharp5", "mS5" },
      minor6 = { "min6", "m6" },
      minorSixNine = { "minor69", "min69", "minSixNine", "m69", "mSixNine", "m6by9" },
   
      minor7flat5 = { "minor7f5", "min7flat5", "m7flat5", "m7f5" },
      minor7sharp5 = { "minor7s5", "min7sharp5", "m7sharp5", "m7s5" },
      minor7flat9 = { "minor7f9", "min7flat9", "m7flat9", "min7f9", "m7f9" },
      minor7sharp9 = { "minor7s9", "min7sharp9", "m7sharp9", "min7s9", "m7s9" },
      minor9sharp5 = { "minor9s5", "min9sharp5", "min9s5", "m9sharp5", "m9s5" },
      minor7sharp5flat9 = "m7sharp5flat9",
      minor11sharp = "m11s",
   
      sevenSus2 = "7sus2",
      sevenSus4 = "7sus4",
      nineSus4 = { "ninesus4", "9sus4" },
      sevenFlat10 = "7f10",
      nineSharp5 = { "9sharp5", "9s5" },
      sevenSharp5flat9 = "7s5f9",
      elevenSharp = "11s",
   
      minorMajor7 = { "minMaj7", "mmaj7" },
   }
   
   local alias_lookup = {}
   
   for k, v in pairs(alias) do
      if type(v) == "table" then
         for _, al in ipairs(v) do
            alias_lookup[al] = k
         end
      else
         alias_lookup[v] = k
      end
   end
   
   setmetatable(chordTable, {
      __index = function(t, k)
         return t[alias_lookup[k]]
      end,
   })
   
   local token = function(id)
      return Ct(Cc(id) * C(V(id)))
   end
   local note = token "note"
   local chordname = token "chordname"
   local chordmods = token "chordmods"
   local notename = token "notename"
   local notemods = token "notemods"
   local range = token "range"
   local open = token "open"
   local drop = token "drop"
   local invert = token "invert"
   local offset = token "offset"
   local octave = token "octave"
   local number = token "number"
   local sep = V "sep"
   
   local grammar = {
      [1] = "chord",
      chord = note * sep ^ -1 * chordname ^ -1 * chordmods ^ -1,
      note = notename * notemods ^ -1,
      chordname = R("az", "09") ^ 1,
      chordmods = (sep * (range + open + drop + invert)) ^ 0,
      notename = R "ag",
      notemods = offset ^ -1 * octave ^ -1,
      offset = S "sfn",
      octave = R "05",
      range = number,
      open = P "o",
      drop = P "d" * number,
      invert = P "i" * number,
      number = R "09",
      sep = P "'",
   }
   
   grammar = Ct(C(grammar))
   
   local notes = { c = 0, d = 2, e = 4, f = 5, g = 7, a = 9, b = 11 }
   
   open = function(chord)
      chord[1] = chord[1] - 12
      chord[3] = chord[3] - 12
      return chord
   end
   
   drop = function(n, chord)
      chord = qsort(chord)
      local index = #chord - (n - 1)
      chord[index] = chord[index] - 12
      return chord
   end
   
   invert = function(n, chord)
      chord = qsort(chord)
      for i = 1, n do
         local index = i % #chord
         if index == 0 then
            index = #chord
         end
         chord[index] = chord[index] + 12
      end
      return chord
   end
   
   range = function(n, chord)
      local new_tones = {}
      n = tonumber(n)
      if #chord > n then
         local acc = {}
         for i = 1, n < 0 and #chord + n or n do
            acc[i] = chord[i]
         end
         return acc
      else
         for i = #chord + 1, n do
            local index = i % #chord
            octave = math.ceil(i / #chord) - 1
            if index == 0 then
               index = #chord
            end
            local new_tone = chord[index] + (12 * octave)
            new_tones[#new_tones + 1] = new_tone
         end
         return concat(chord, new_tones)
      end
   end
   
   local parseChord = function(chord)
      if type(chord) == "number" then
         return chord
      end
      local ast = grammar:match(chord)
      if not ast then
         return false
      end
      notename = notes[ast[2][3][2]]
      offset = 0
      octave = 5
      if ast[2][4] ~= nil then
         local mods = ast[2][4]
         local _max_0 = #mods
         for _index_0 = 3, _max_0 < 0 and #mods + _max_0 or _max_0 do
            local mod = mods[_index_0]
            if mod[1] == "offset" then
               local _exp_0 = mod[2]
               if "s" == _exp_0 then
                  offset = 1
               elseif "f" == _exp_0 then
                  offset = -1
               else
                  offset = 0
               end
            end
            if mod[1] == "octave" then
               octave = tonumber(mod[2])
            end
         end
      end
      local rootnote = notename + offset + (octave - 5) * 12
      if ast[3][2] == "" then
         return rootnote
      end
      local chordtable = chordTable[ast[3][2]]
      chordtable = map(function(x)
         return x + rootnote
      end, chordtable)
      if ast[4][2] ~= "" then
         local _list_0 = ast[4]
         local _max_0 = #ast[4]
         for _index_0 = 3, _max_0 < 0 and #_list_0 + _max_0 or _max_0 do
            local mod = _list_0[_index_0]
            if mod[1] == "open" then
               chordtable = open(chordtable)
            end
            if mod[1] == "drop" then
               chordtable = drop(mod[3][2], chordtable)
            end
            if mod[1] == "range" then
               chordtable = range(mod[3][2], chordtable)
            end
            if mod[1] == "invert" then
               chordtable = invert(mod[3][2], chordtable)
            end
         end
      end
      return chordtable
   end
   
   ---@enum (key) Scales
   local scaleTable = {
      minPent = { 0, 3, 5, 7, 10 },
      majPent = { 0, 2, 4, 7, 9 },
      ritusen = { 0, 2, 5, 7, 9 },
      egyptian = { 0, 2, 5, 7, 10 },
      kumai = { 0, 2, 3, 7, 9 },
      hirajoshi = { 0, 2, 3, 7, 8 },
      iwato = { 0, 1, 5, 6, 10 },
      chinese = { 0, 4, 6, 7, 11 },
      indian = { 0, 4, 5, 7, 10 },
      pelog = { 0, 1, 3, 7, 8 },
      prometheus = { 0, 2, 4, 6, 11 },
      scriabin = { 0, 1, 4, 7, 9 },
      gong = { 0, 2, 4, 7, 9 },
      shang = { 0, 2, 5, 7, 10 },
      jiao = { 0, 3, 5, 8, 10 },
      zhi = { 0, 2, 5, 7, 9 },
      yu = { 0, 3, 5, 7, 10 },
      whole = { 0, 2, 4, 6, 8, 10 },
      augmented = { 0, 3, 4, 7, 8, 11 },
      augmented2 = { 0, 1, 4, 5, 8, 9 },
      hexMajor7 = { 0, 2, 4, 7, 9, 11 },
      hexDorian = { 0, 2, 3, 5, 7, 10 },
      hexPhrygian = { 0, 1, 3, 5, 8, 10 },
      hexSus = { 0, 2, 5, 7, 9, 10 },
      hexMajor6 = { 0, 2, 4, 5, 7, 9 },
      hexAeolian = { 0, 3, 5, 7, 8, 10 },
      major = { 0, 2, 4, 5, 7, 9, 11 },
      ionian = { 0, 2, 4, 5, 7, 9, 11 },
      dorian = { 0, 2, 3, 5, 7, 9, 10 },
      phrygian = { 0, 1, 3, 5, 7, 8, 10 },
      lydian = { 0, 2, 4, 6, 7, 9, 11 },
      mixolydian = { 0, 2, 4, 5, 7, 9, 10 },
      aeolian = { 0, 2, 3, 5, 7, 8, 10 },
      minor = { 0, 2, 3, 5, 7, 8, 10 },
      locrian = { 0, 1, 3, 5, 6, 8, 10 },
      harmonicMinor = { 0, 2, 3, 5, 7, 8, 11 },
      harmonicMajor = { 0, 2, 4, 5, 7, 8, 11 },
      melodicMinor = { 0, 2, 3, 5, 7, 9, 11 },
      melodicMinorDesc = { 0, 2, 3, 5, 7, 8, 10 },
      melodicMajor = { 0, 2, 4, 5, 7, 8, 10 },
      bartok = { 0, 2, 4, 5, 7, 8, 10 },
      hindu = { 0, 2, 4, 5, 7, 8, 10 },
      todi = { 0, 1, 3, 6, 7, 8, 11 },
      purvi = { 0, 1, 4, 6, 7, 8, 11 },
      marva = { 0, 1, 4, 6, 7, 9, 11 },
      bhairav = { 0, 1, 4, 5, 7, 8, 11 },
      ahirbhairav = { 0, 1, 4, 5, 7, 9, 10 },
      superLocrian = { 0, 1, 3, 4, 6, 8, 10 },
      romanianMinor = { 0, 2, 3, 6, 7, 9, 10 },
      hungarianMinor = { 0, 2, 3, 6, 7, 8, 11 },
      neapolitanMinor = { 0, 1, 3, 5, 7, 8, 11 },
      enigmatic = { 0, 1, 4, 6, 8, 10, 11 },
      spanish = { 0, 1, 4, 5, 7, 8, 10 },
      leadingWhole = { 0, 2, 4, 6, 8, 10, 11 },
      lydianMinor = { 0, 2, 4, 6, 7, 8, 10 },
      neapolitanMajor = { 0, 1, 3, 5, 7, 9, 11 },
      locrianMajor = { 0, 2, 4, 5, 6, 8, 10 },
      diminished = { 0, 1, 3, 4, 6, 7, 9, 10 },
      diminished2 = { 0, 2, 3, 5, 6, 8, 9, 11 },
      messiaen1 = { 0, 2, 4, 6, 8, 10 },
      messiaen2 = { 0, 1, 3, 4, 6, 7, 9, 10 },
      messiaen3 = { 0, 2, 3, 4, 6, 7, 8, 10, 11 },
      messiaen4 = { 0, 1, 2, 5, 6, 7, 8, 11 },
      messiaen5 = { 0, 1, 5, 6, 7, 11 },
      messiaen6 = { 0, 2, 4, 5, 6, 8, 10, 11 },
      messiaen7 = { 0, 1, 2, 3, 5, 6, 7, 8, 9, 11 },
      bayati = { 0, 1.5, 3, 5, 7, 8, 10 },
      hijaz = { 0, 1, 4, 5, 7, 8.5, 10 },
      sikah = { 0, 1.5, 3.5, 5.5, 7, 8.5, 10.5 },
      rast = { 0, 2, 3.5, 5, 7, 9, 10.5 },
      iraq = { 0, 1.5, 3.5, 5, 6.5, 8.5, 10.5 },
      saba = { 0, 1.5, 3, 4, 6, 8, 10 },
      chromatic = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 },
   }
   
   local floor = math.floor
   
   local getScale = function(name)
      return function(num)
         local istab = false
         if type(num) == "table" and num.note then
            num = num.note
            istab = true
         end
         num = tonumber(num)
         local scale = scaleTable[name]
         local index = (num + 1) % #scale
         local octave = floor(num / #scale)
         if index == 0 then
            index = #scale
         end
         local note = scale[index] + octave * 12
         if istab then
            return { ["note"] = note }
         end
         return note
      end
   end
   
   local flatten, zipWith, splitAt, rotate = ut.flatten, ut.zipWith, ut.splitAt, ut.rotate
   local min = math.min
   
   local function left(n, m)
      local ons, offs = n[1], n[2]
      local xs, ys = m[1], m[2]
      local _xs, __xs = splitAt(offs, xs)
      return { offs, ons - offs }, { zipWith(concat, _xs, ys), __xs }
   end
   
   local function right(n, m)
      local ons, offs = n[1], n[2]
      local xs, ys = m[1], m[2]
      local _ys, __ys = splitAt(ons, ys)
      return { ons, offs - ons }, { zipWith(concat, xs, _ys), __ys }
   end
   
   local function _bjork(n, m)
      local ons, offs = n[1], n[2]
      if min(ons, offs) <= 1 then
         return { n, m }
      else
         if ons > offs then
            return _bjork(left(n, m))
         else
            return _bjork(right(n, m))
         end
      end
   end
   
   local function bjork(ons, steps, offset)
      offset = offset or 0
      local offs = steps - ons
      local x, y = {}, {}
      for i = 1, ons do
         x[i] = { true }
      end
      for i = 1, offs do
         y[i] = { false }
      end
      local result = _bjork({ ons, offs }, { x, y })
      result = concat(flatten(result[2][1]), flatten(result[2][2]))
      return rotate(offset, result)
   end
   
   theory = { getScale = getScale, parseChord = parseChord, bjork = bjork }
   
end

do
   _G.struct = nil
   local Stream = types.Stream
   
   local floor = math.floor
   local type = type
   local pairs = pairs
   
   local sleep = function(sec)
      return socket.sleep(sec)
   end
   
   local target = {
      name = "SuperDirt",
      address = "127.0.0.1",
      port = 57120,
      latency = 0.2,
      handshake = true,
   }
   
   local typeMap = { table = "b", number = "f", string = "s" }
   
   local typesString = function(msg)
      local ts = ""
      for i = 1, #msg do
         local x = msg[i]
         if typeMap[type(x)] then
            ts = ts .. typeMap[type(x)]
         else
            ts = ts .. "b"
         end
      end
      return ts
   end
   
   local Timetag = losc.Timetag
   local Pattern = losc.Pattern
   local Packet = losc.Packet
   
   local M = {}
   M.__index = M
   --- Fractional precision for bundle scheduling.
   -- 1000 is milliseconds. 1000000 is microseconds etc. Any precision is valid
   -- that makes sense for the plugin's scheduling function.
   M.precision = 1000
   
   --- Create a new instance.
   -- @tparam[options] table options Options.
   -- @usage local udp = plugin.new()
   -- @usage
   -- local udp = plugin.new {
   --   sendAddr = '127.0.0.1',
   --   sendPort = 9000,
   --   recvAddr = '127.0.0.1',
   --   recvPort = 8000,
   --   ignore_late = true, -- ignore late bundles
   -- }
   function M.new(options)
      local self = setmetatable({}, M)
      self.options = options or {}
      self.handle = uv.new_udp "inet"
      assert(self.handle, "Could not create UDP handle.")
      return self
   end
   
   --- Create a Timetag with the current time.
   -- Precision is in milliseconds.
   -- @return Timetag object with current time.
   function M:now() -- luacheck: ignore
      local s, m = uv.gettimeofday()
      return Timetag.new(s, m / M.precision)
   end
   
   --- Schedule a OSC method for dispatch.
   --
   -- @tparam number timestamp When to schedule the bundle.
   -- @tparam function handler The OSC handler to call.
   function M:schedule(timestamp, handler) -- luacheck: ignore
      timestamp = math.max(0, timestamp)
      if timestamp > 0 then
         local timer = uv.new_timer()
         timer:start(timestamp, 0, handler)
      else
         handler()
      end
   end
   
   --- Start UDP server.
   -- This function is blocking.
   -- @tparam string host IP address (e.g. '127.0.0.1').
   -- @tparam number port The port to listen on.
   function M:open(host, port)
      host = host or self.options.recvAddr
      port = port or self.options.recvPort
      self.handle:bind(host, port, { reuseaddr = true })
      self.handle:recv_start(function(err, data, addr)
         assert(not err, err)
         if data then
            self.remote_info = addr
            local ok, errormsg = pcall(Pattern.dispatch, data, self)
            if not ok then
               print(errormsg)
            end
         end
      end)
      -- updated if port 0 is passed in as default (chooses a random port)
      self.options.recvPort = self.handle:getsockname().port
   end
   
   function M:run_non_blocking()
      print "listening"
      -- Run the event loop once and return
      uv.run "nowait"
   end
   
   --- Close UDP server.
   function M:close()
      self.handle:recv_stop()
      if not self.handle:is_closing() then
         self.handle:close()
      end
      uv.walk(uv.close)
   end
   
   --- Send a OSC packet.
   -- @tparam table packet The packet to send.
   -- @tparam[opt] string address The IP address to send to.
   -- @tparam[opt] number port The port to send to.
   function M:send(packet, address, port)
      address = address or self.options.sendAddr
      port = port or self.options.sendPort
      packet = assert(Packet.pack(packet))
      self.handle:try_send(packet, address, port)
   end
   
   local osc, sendOSC
   local udp = M.new {
      recvAddr = "127.0.0.1",
      recvPort = 9001,
      sendPort = target.port,
      sendAddr = target.address,
      -- ignore_late = true, -- ignore late bundles
   }
   osc = losc.new { plugin = udp }
   
   sendOSC = function(value, ts)
      local msg = {}
      for key, val in pairs(value) do
         msg[#msg + 1] = key
         msg[#msg + 1] = val
      end
      msg.types = typesString(msg)
      msg.address = "/dirt/play"
      local b = osc.new_message(msg)
      -- local b = osc.new_bundle(ts, osc.new_message(msg))
      osc:send(b)
   end
   sendOSC { 1, 2, "sda" }
   
   osc:add_handler("/ctrl", function(data)
      print(ut.dump(data))
   end)
   
   osc:add_handler("/param/{x,y,z}", function(data)
      print(ut.dump(data))
   end)
   
   local mt = { __class = "clock" }
   
   function mt:start()
      if not self.running then
         self.running = true
         osc:open() -- ???
         return self:createNotifyCoroutine()
      end
   end
   
   function mt:stop()
      self.running = false
      print "Clock: stopped"
   end
   
   function mt:subscribe(key, pattern)
      if not self.subscribers[key] then
         self.subscribers[key] = Stream(self.callback)
      end
      self.subscribers[key].pattern = pattern
   end
   
   function mt:unsubscribe(key)
      self.subscribers[key] = nil
   end
   
   function mt:setbpm(bpm)
      self.sessionState:set_tempo(bpm, 0)
      self.link:commit_audio_session_state(self.sessionState)
   end
   
   function mt:setcps(cps)
      self.sessionState:set_tempo(cps * self.beatsPerCycle * 60, 0)
      self.link:commit_audio_session_state(self.sessionState)
   end
   
   function mt:createNotifyCoroutine()
      self.co = coroutine.create(function(f)
         local start = self.link:clock_micros()
         local ticks = 0
         local mill = 1000000
         local frame = self.sampleRate * mill
         while self.running do
            uv.run "nowait"
            ticks = ticks + 1
            local logicalNow = floor(start + (ticks * frame))
            local logicalNext = floor(start + ((ticks + 1) * frame))
            local now = self.link:clock_micros()
            local wait = (logicalNow - now) / mill
            if wait > 0 then
               sleep(wait)
            end
            if not self.running then
               break
            end
            self.link:capture_audio_session_state(self.sessionState)
            local cps = (self.sessionState:tempo() / self.beatsPerCycle) / 60
            local cycleFrom = self.sessionState:beat_at_time(logicalNow, 0) / self.beatsPerCycle
            local cycleTo = self.sessionState:beat_at_time(logicalNext, 0) / self.beatsPerCycle
            -- print(string.format("cycleFrom : %d;  cycleTo : %d", cycleFrom, cycleTo))
            if f then
               f()
            end
            for _, sub in pairs(self.subscribers) do
               sub:notifyTick(cycleFrom, cycleTo, self.sessionState, cps, self.beatsPerCycle, mill, now)
            end
            coroutine.yield()
         end
         self.linkEnabled = false
      end)
   end
   
   mt.__index = mt
   
   function Clock(bpm, sampleRate, beatsPerCycle, callback)
      bpm = bpm or 120
      sampleRate = sampleRate or (1 / 20)
      beatsPerCycle = beatsPerCycle or 4
      callback = callback or sendOSC
      return setmetatable({
         callback = callback,
         bpm = bpm,
         sampleRate = sampleRate,
         beatsPerCycle = beatsPerCycle,
         link = has_al and al.create(bpm) or {}, -- HACK:
         sessionState = has_al and al.create_session_state() or {},
         subscribers = {},
         running = false,
         latency = 0.2,
      }, mt)
   end
   
end

do
   local DefaultClock = Clock()
   
   function factory.p(key, pattern)
      DefaultClock:subscribe(key, pattern)
      return pattern
   end
   
   -- TODO: cause server to freeze ...
   function factory._p(key)
      DefaultClock:unsubscribe(key)
   end
   
   factory.p_ = factory._p
   
   function factory.hush()
      for i, _ in pairs(DefaultClock.subscribers) do
         DefaultClock:unsubscribe(i)
      end
   end
   
   -- function M.panic()
   --    M.hush()
   --    once(s "superpanic")
   -- end
   -- panic :: Tidally => IO ()
   -- panic = hush >> once (sound "superpanic")
   
   for i = 1, 16 do
      if i <= 12 then
         factory["d" .. i] = function(a)
            return factory.p(i, a:orbit(i - 1))
         end
      else
         factory["d" .. i] = function(a)
            return factory.p(i, a)
         end
      end
      factory["_d" .. i] = function()
         return factory._p(i)
      end
      factory["d" .. i .. "_"] = function()
         return factory._p(i)
      end
   end
   
   factory.DefaultClock = DefaultClock
   
   function factory.setcps(cps)
      DefaultClock:setcps(cps)
   end
   
   function factory.setbpm(bpm)
      DefaultClock:setbpm(bpm)
   end
   
   factory.bpm = factory.setbpm
   factory.cps = factory.setcps
   
end

do
   control.genericParams = {
      { "s", "n", "gain" },
      { "cutoff", "resonance" },
      { "hcutoff", "hresonance" },
   
      { "delay", "delaytime", "delayfeedback" },
      { "room", "size" },
      { "bandf", "bandq" }, --bpenv
      "toArg",
      "from",
      "to",
      "accelerate",
      "amp",
      "attack",
      "bandq",
      "begin",
      "legato",
      "clhatdecay",
      "crush",
      "coarse",
      "channel",
      "cut",
      "cutoff",
      "cutoffegint",
      "decay",
      "delayfeedback",
      "delaytime",
      "detune",
      "djf",
      "dry",
      "end",
      "fadeTime",
      "fadeInTime",
      "freq",
      "gain",
      "gate",
      "hatgrain",
      "hold",
      "hresonance",
      "lagogo",
      "lclap",
      "lclaves",
      "lclhat",
      "lcrash",
      "leslie",
      "lrate",
      "lfodelay",
      "lfosync",
      "lock",
      "metatune",
      "mtranspose",
      "octaveR",
      "ophatdecay",
      "orbit",
      "pan",
      "panorient",
      "portamento",
      "sagogo",
      "semitone",
      "speed",
      "sustain",
      "unit",
      "voice",
      "vowel",
      "modwheel",
      "tremolorate",
      "fshiftnote",
      "kcutoff",
      "octer",
      "octersub",
      "octersubsub",
      "ring",
      "ringf",
      "ringdf",
      "distort",
      "freeze",
      "xsdelay",
      "tsdelay",
      "real",
      "imag",
      "enhance",
      "partials",
      "comb",
      "smear",
      "scram",
      "binshift",
      "hbrick",
      "lbrick",
   }
   
   control.aliasParams = {
      s = "sound",
      note = "up",
      attack = "att",
      bandf = "bpf",
      bandq = "bpq",
      clhatdecay = "chdecay",
      cutoff = { "ctf", "lpf" },
      cutoffegint = "ctfg",
      delayfeedback = { "dfb", "delayfb" },
      delaytime = { "dt", "delayt" },
      detune = "det",
      fadeTime = "fadeOutTime",
      gate = "gat",
      hatgrain = "hg",
      hcutoff = "hpf",
      hresonance = "hpq",
      lagogo = "lag",
      lkick = "lbd",
      lclhat = "lch",
      lclaves = "lcl",
      lclap = "lcp",
      lcrash = "lcr",
      lfocutoffint = "lfoc",
      lfoint = "lfoi",
      lfopitchint = "lfop",
      lhitom = "lht",
      llotom = "llt",
      lophat = "loh",
      resonance = "lpq",
      lsnare = "lsn",
      n = "number",
      ophatdecay = "ohdecay",
      phaserdepth = "phasdp",
      phaserrate = "phasr",
      pitch1 = "pit1",
      pitch2 = "pit2",
      pitch3 = "pit3",
      portamento = "por",
      release = "rel",
      sagogo = "sag",
      sclaves = "scl",
      sclap = "scp",
      scrash = "scr",
      size = "sz",
      slide = "sld",
      stutterdepth = "std",
      stuttertime = "stt",
      sustain = "sus",
      tomdecay = "tdecay",
      tremolodepth = "tremdp",
      tremolorate = "tremr",
      vcfegint = "vcf",
      vcoegint = "vco",
      voice = "voi",
   }
   
end

do
   
   local bjork, getScale = theory.bjork, theory.getScale
   local Event, Span, Time, TDef, ValueMap = types.Event, types.Span, types.Time, types.TDef, types.ValueMap
   
   local unpack = unpack or rawget(table, "unpack")
   local pairs = pairs
   local ipairs = ipairs
   local setmetatable = setmetatable
   local tconcat = table.concat
   local tremove = table.remove
   local str_format = string.format
   local sin = math.sin
   local min = math.min
   local max = math.max
   local pi = math.pi
   local floor = math.floor
   local is_array = ut.is_array
   local reduce = ut.reduce
   local map = ut.map
   local id = ut.id
   local filter = ut.filter
   local dump = ut.dump
   local curry = ut.curry
   local union = ut.union
   local concat = ut.concat
   local flip = ut.flip
   local method_wrap = ut.method_wrap
   local curry_wrap = ut.curry_wrap
   local get_args = ut.get_args
   local timeToRand = ut.timeToRand
   local memoize = ut.memoize
   local T = ut.T
   
   local fast, pure, fastcat, slowcat, stack, silence, focus, range, rev, compress
   
   local TYPES = {}
   local op = {}
   
   -- give mini access to global vars
   setmetatable(pattern, { __index = _G })
   
   local eval = notation.mini(pattern)
   local reify = memoize(function(thing)
      return ut.switch()
         :case("string")
         :call(function()
            local res = eval(thing)
            return res and res or silence
         end)
         :case("table")
         :call(function()
            if is_array(thing) then
               return fastcat(thing)
            else
               return pure(ValueMap(thing))
            end
         end)
         :case("pattern")
         :call(function()
            return thing
         end)
         :default(function()
            return pure(thing)
         end)(T(thing))
   end)
   pattern.reify = reify
   
   local mt = { __class = "pattern" }
   
   function mt:len()
      return #(self(0, 1))
   end
   
   function mt:__call(b, e, controls)
      local span = Span(b, e)
      span.controls = controls
      return setmetatable(self.query(span), {
         __tostring = function(t)
            return dump(t)
         end,
      })
   end
   
   function mt:__tostring()
      return dump(self(0, 1))
   end
   
   function mt:show()
      return tostring(self)
   end
   
   -- TODO: not triggered in busted
   function mt:__eq(other)
      return self:__tostring() == other:__tostring()
   end
   
   function mt:__concat(other)
      return op["|>"](self, other)
   end
   
   function mt:__add(other)
      return op["|+"](self, other)
   end
   
   function mt:__sub(other)
      return op["|-"](self, other)
   end
   
   function mt:__mul(other)
      return op["|*"](self, other)
   end
   
   function mt:__div(other)
      return op["|/"](self, other)
   end
   
   function mt:__mod(other)
      return op["|%"](self, other)
   end
   
   function mt:__pow(other)
      return op["|^"](self, other)
   end
   
   function mt:slowcat(pats)
      pats[#pats + 1] = self
      return slowcat(pats)
   end
   
   -- TODO: intuitive??
   function mt:fastcat(pats)
      pats[#pats + 1] = self
      return fastcat(pats)
   end
   
   function mt:stack(pats)
      pats[#pats + 1] = self
      return stack(pats)
   end
   
   mt.__index = mt
   
   ---@class Pattern
   local function Pattern(query)
      query = query or function()
         return {}
      end
      return setmetatable({ query = query }, mt)
   end
   pattern.Pattern = Pattern
   
   local function filterEvents(pat, func)
      local query = function(span)
         local events = pat.query(span)
         return filter(func, events)
      end
      return Pattern(query)
   end
   mt.filterEvents = filterEvents
   
   local function filterValues(pat, condf)
      local query = function(span)
         local events = pat.query(span)
         local f = function(event)
            return condf(event.value)
         end
         return filter(f, events)
      end
      return Pattern(query)
   end
   mt.filterValues = filterValues
   
   local function removeNils(pat)
      return filterValues(pat, function(v)
         return v ~= nil
      end)
   end
   mt.removeNils = removeNils
   
   local function splitQueries(pat)
      local query = function(span)
         local cycles = span:spanCycles()
         local res = {}
         for i = 1, #cycles do
            local evs = pat.query(cycles[i])
            for j = 1, #evs do
               res[#res + 1] = evs[j]
            end
         end
         return res
      end
      return Pattern(query)
   end
   mt.splitQueries = splitQueries
   
   local function withValue(pat, f)
      local query = function(span)
         local events = pat.query(span)
         for i = 1, #events do
            events[i] = events[i]:withValue(f)
         end
         return events
      end
      return Pattern(query)
   end
   mt.withValue = withValue
   
   local fmap = withValue
   mt.fmap = fmap
   
   local function withQuerySpan(pat, f)
      local query = function(span)
         return pat.query(f(span))
      end
      return Pattern(query)
   end
   mt.withQuerySpan = withQuerySpan
   
   local function withQueryTime(pat, f)
      return withQuerySpan(pat, function(span)
         return span:withTime(f)
      end)
   end
   mt.withQueryTime = withQueryTime
   
   local function withEvents(pat, f)
      return Pattern(function(span)
         return f(pat.query(span))
      end)
   end
   mt.withEvents = withEvents
   
   local function withEvent(pat, f)
      return withEvents(pat, function(events)
         for i = 1, #events do
            events[i] = f(events[i])
         end
         return events
      end)
   end
   mt.withEvent = withEvent
   
   local function withEventSpan(pat, f)
      local query = function(span)
         local events = pat.query(span)
         for i = 1, #events do
            events[i] = events[i]:withSpan(f)
         end
         return events
      end
      return Pattern(query)
   end
   mt.withEventSpan = withEventSpan
   
   local function withEventTime(pat, f)
      local query = function(span)
         local events = pat.query(span)
         local time_func = function(span)
            return span:withTime(f)
         end
         local event_func = function(event)
            return event:withSpan(time_func)
         end
         for i = 1, #events do
            events[i] = event_func(events[i])
         end
         return events
      end
      return Pattern(query)
   end
   mt.withEventTime = withEventTime
   
   local function withTime(pat, qf, ef)
      local query = withQueryTime(pat, qf)
      local pattern = withEventTime(query, ef)
      return pattern
   end
   mt.withTime = withTime
   
   local function onsetsOnly(pat)
      return filterEvents(pat, function(event)
         return event:hasOnset()
      end)
   end
   mt.onsetsOnly = onsetsOnly
   
   local function discreteOnly(pat)
      return filterEvents(pat, function(event)
         return event.whole
      end)
   end
   mt.discreteOnly = discreteOnly
   
   local function appWhole(pat, whole_func, pat_val)
      local query = function(span)
         local event_funcs = pat.query(span)
         local event_vals = pat_val.query(span)
         local apply = function(event_func, event_val)
            local new_part = event_func.part:sect(event_val.part)
            if not new_part then
               return
            end
            return Event(whole_func(event_func.whole, event_val.whole), new_part, event_func.value(event_val.value))
         end
         local events = {}
         for _, ef in pairs(event_funcs) do
            for _, ev in ipairs(event_vals) do
               events[#events + 1] = apply(ef, ev)
            end
         end
         return events
      end
      return Pattern(query)
   end
   mt.appWhole = appWhole
   
   -- Tidal's <*>
   local function appBoth(pat, pat_val)
      local whole_func = function(span_a, span_b)
         if not span_a or not span_b then
            return
         end
         return span_a:sect(span_b)
      end
      return appWhole(pat, whole_func, pat_val)
   end
   mt.appBoth = appBoth
   
   -- Tidal's <*
   local function appLeft(pat, pat_val)
      local query = function(span)
         local events = {}
         local event_funcs = pat.query(span)
         for _, event_func in ipairs(event_funcs) do
            local whole = event_func:wholeOrPart()
            local event_vals = pat_val.query(whole)
            for _, event_val in ipairs(event_vals) do
               local new_whole = event_func.whole
               local new_part = event_func.part:sect(event_val.part)
               if new_part then
                  local new_value = event_func.value(event_val.value)
                  events[#events + 1] = Event(new_whole, new_part, new_value)
               end
            end
         end
         return events
      end
      return Pattern(query)
   end
   mt.appLeft = appLeft
   
   -- Tidal's *>
   local function appRight(pat, pat_val)
      local query = function(span)
         local events = {}
         local event_vals = pat_val.query(span)
         for _, event_val in ipairs(event_vals) do
            local whole = event_val:wholeOrPart()
            local event_funcs = pat.query(whole)
            for _, event_func in ipairs(event_funcs) do
               local new_whole = event_val.whole
               local new_part = event_func.part:sect(event_val.part)
               if new_part then
                  local new_value = event_func.value(event_val.value)
                  events[#events + 1] = Event(new_whole, new_part, new_value)
               end
            end
         end
         return events
      end
      return Pattern(query)
   end
   mt.appRight = appRight
   
   local function bindWhole(pat, choose_whole, func)
      local query = function(span)
         local events = pat.query(span)
         local res = {}
         for _, a in ipairs(events) do
            local evs = func(a.value).query(a.part)
            for _, b in ipairs(evs) do
               res[#res + 1] = Event(choose_whole(a.whole, b.whole), b.part, b.value)
            end
         end
         return res
      end
      return Pattern(query)
   end
   
   local function bind(pat, func)
      local whole_func = function(a, b)
         if a == nil or b == nil then
            return nil
         end
         return a:sect(b)
      end
      return bindWhole(pat, whole_func, func)
   end
   
   local function join(pat)
      return bind(pat, id)
   end
   
   local function outerBind(pat, func)
      return bindWhole(pat, function(a, _)
         return a
      end, func)
   end
   
   local function innerBind(pat, func)
      return bindWhole(pat, function(_, b)
         return b
      end, func)
   end
   
   local function outerJoin(pat)
      return outerBind(pat, id)
   end
   
   local function innerJoin(pat)
      return innerBind(pat, id)
   end
   
   local function squeezeJoin(pat)
      local query = function(span)
         local events = discreteOnly(pat).query(span)
         local flatEvent = function(outerEvent)
            local span = outerEvent:wholeOrPart()
            local innerPat = pattern.focus(span.start, span.stop, outerEvent.value)
            local innerEvents = innerPat.query(outerEvent.part)
            local munge = function(outer, inner)
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
            for i = 1, #innerEvents do
               innerEvents[i] = munge(outerEvent, innerEvents[i])
            end
            return innerEvents
         end
         local result = {}
         for i = 1, #events do
            local evs = flatEvent(events[i])
            for j = 1, #evs do
               result[#result + 1] = evs[j]
            end
         end
         return filter(function(x)
            return x
         end, result)
      end
      return Pattern(query)
   end
   
   local function squeezeBind(pat, func)
      return squeezeJoin(fmap(pat, func))
   end
   
   local _op = {}
   function _op.In(f)
      return function(a, b)
         a, b = fmap(reify(a), curry(f, 2)), reify(b)
         return appLeft(a, b):removeNils()
      end
   end
   
   function _op.Out(f)
      return function(a, b)
         a, b = fmap(reify(a), curry(f, 2)), reify(b)
         return appRight(a, b):removeNils()
      end
   end
   
   function _op.Mix(f)
      return function(a, b)
         a, b = fmap(reify(a), curry(f, 2)), reify(b)
         return appBoth(a, b):removeNils()
      end
   end
   
   function _op.Squeeze(f)
      return function(a, b)
         return squeezeJoin(fmap(reify(a), function(c)
            return fmap(reify(b), function(d)
               return f(c, d)
            end)
         end)):removeNils()
      end
   end
   
   function _op.SqueezeOut(f)
      return function(a, b)
         return squeezeJoin(fmap(reify(b), function(c)
            return fmap(reify(a), function(d)
               return f(d, c)
            end)
         end)):removeNils()
      end
   end
   
   -- stylua: ignore start
   local ops = {
      set = function(_, b) return b end,
      add = function(a, b) return a + b end,
      sub = function(a, b) return a - b end,
      mul = function(a, b) return a * b end,
      div = function(a, b) return a / b end,
      mod = function(a, b) return a % b end,
      pow = function(a, b) return a ^ b end,
      concat = function (a, b) return a .. b end,
      keepif = function (a, b)
         if b == 0 then b = false end
         return b and a or nil
      end,
      uni = function (a, b) return union(a, b) end,
      funi = function (a, b) return flip(union)(a, b) end,
   }
   -- stylua: ignore end
   
   -- local hows = { "In", "Out", "Mix", "Squeeze", "Squeezeout", "Trig", "Trigzero" }
   local hows = { "In", "Out", "Mix", "Squeeze", "SqueezeOut" }
   local op_set = {
      add = "+",
      sub = "-",
      mul = "*",
      div = "/",
      mod = "%",
      pow = "^",
      keepif = "?",
      concat = "..", -- ?
      uni = "<",
      funi = ">",
   }
   
   local how_format = {
      In = "|%s",
      Out = "%s|",
      Mix = "|%s|",
      Squeeze = "||%s",
      SqueezeOut = "%s||",
   }
   
   for k, f in pairs(ops) do
      op[k] = {}
      for _, v in ipairs(hows) do
         op[k][v] = _op[v](f)
         if op_set[k] and how_format[v] then
            local symb = str_format(how_format[v], op_set[k])
            op[symb] = _op[v](f)
         end
      end
   end
   op["#"] = op["|>"]
   
   silence = Pattern()
   pattern.silence = silence
   
   function pure(value)
      local query = function(span)
         local cycles = span:spanCycles()
         for i, v in ipairs(cycles) do
            cycles[i] = Event(v.start:wholeCycle(), v, value)
         end
         return cycles
      end
      return Pattern(query)
   end
   pattern.pure = pure
   
   local function purify(value)
      if T(value) == "pattern" then
         return value
      else
         return pure(value)
      end
   end
   
   local function patternify(arity, func)
      return function(...)
         local pats = { ... }
         local pat = tremove(pats, #pats)
         if arity == 1 then
            return func(pat)
         end
         local left = tremove(pats, 1)
         local mapFn = function(...)
            local args = { ... }
            args[#args + 1] = pat
            return func(unpack(args))
         end
         mapFn = curry(mapFn, arity - 1)
         return innerJoin(reduce(appLeft, fmap(left, mapFn), pats))
      end
   end
   
   local function type_wrap(f, name)
      local sig = TYPES[name]
      return function(...)
         local args = { ... }
         for i, v in ipairs(args) do
            local t = sig[i]
            local tc, tvar, istable = t.constructor, t[1], t.istable
            if istable then
               for j, vv in ipairs(v) do
                  if tc then
                     if tc == "Pattern" then
                        v[j] = purify(vv) -- for fastcat and slowcat ...
                     end
                  end
               end
            else
               if tvar == "Time" then
                  v = Time(v)
               end
               if tc then
                  if tc == "Pattern" and tvar == "f" and type(v) == "string" then
                     v = reify("(" .. v .. ")")
                  elseif tc == "Pattern" then
                     v = reify(v)
                  end
               end
               args[i] = v
            end
         end
         return f(unpack(args))
      end
   end
   
   local function register(type_sig, f, nify)
      local tdef, name = TDef(type_sig)
      if T(nify) == "nil" then
         nify = true
      end
      local arg_names = get_args(f)
      local arity = #arg_names
      for i, v in pairs(arg_names) do
         tdef[i].name = v
      end
      if nify then
         TYPES[name] = tdef
         local f_p = patternify(arity, f)
         local f_p_t = type_wrap(f_p, name)
         local f_c_p_t = curry_wrap(arity, f_p_t)
         pattern[name] = f_c_p_t
         rawset(mt, name, method_wrap(f_p_t))
      else
         TYPES[name] = tdef
         local f_t = type_wrap(f, name)
         local f_t_c = curry_wrap(arity, f_t)
         pattern[name] = f_t_c
         rawset(mt, name, method_wrap(f_t))
      end
   end
   pattern.register = register
   
   local function overlay(a, b)
      local query = function(st)
         return concat(a.query(st), b.query(st))
      end
      return Pattern(query)
   end
   register("overlay :: Pattern a -> Pattern a -> Pattern a", overlay, false)
   
   function stack(pats)
      return reduce(overlay, silence, pats)
   end
   register("stack :: [Pattern a] -> Pattern a", stack, false)
   
   function pattern.polymeter(steps, pats)
      for i, pat in ipairs(pats) do
         pats[i] = pattern.fast(steps / pat:len(), pat)
      end
      return stack(pats)
   end
   -- register("polymeter :: Pattern Int -> [Pattern a] -> Pattern a", polymeter, false)
   
   function slowcat(pats)
      local query = function(span)
         local cyc = span.start:sam():asFloat()
         local n = #pats
         local i = cyc % n
         local pat = pats[i + 1]
         if not pat then
            return {}
         end
         local offset = cyc - (cyc - i) / n
         return withEventTime(pat, function(t)
            return t + offset
         end).query(span:withTime(function(t)
            return t - offset
         end))
      end
      return splitQueries(Pattern(query))
   end
   register("slowcat :: [Pattern a] -> Pattern a", slowcat, false)
   
   function fastcat(pats)
      return pattern.fast(#pats, pattern.slowcat(pats))
   end
   register("fastcat :: [Pattern a] -> Pattern a", fastcat, false)
   
   local function timecat(tups)
      local total = 0
      for i, v in ipairs(tups) do
         if i % 2 == 1 then
            total = total + v
         end
      end
      local accum = Time(0)
      local pats = {}
      local time, pat, b, e
      for i = 1, #tups, 2 do
         time, pat = tups[i], reify(tups[i + 1])
         b, e = accum / total, (accum + time) / total
         pats[#pats + 1] = compress(b, e, pat)
         accum = accum + time
      end
      return stack(pats)
   end
   pattern.timecat = timecat
   
   local function arrange(tups)
      local total = 0
      for i, v in ipairs(tups) do
         if i % 2 == 1 then
            total = total + v
         end
      end
      local cycles, pat
      for i = 1, #tups, 2 do
         cycles, pat = tups[i], reify(tups[i + 1])
         tups[i + 1] = pattern.fast(cycles, pat)
      end
      return slow(total, timecat(tups))
   end
   pattern.arrange = arrange
   
   local function superimpose(f, pat)
      return overlay(pat, f(pat))
   end
   register("superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a", superimpose, false)
   
   local function layer(tf, pat)
      for i, f in ipairs(tf) do
         tf[i] = f(pat)
      end
      return stack(tf)
   end
   register("layer :: [(Pattern a -> Pattern b)] -> Pattern a -> Pattern b", layer, false) -- a little ugly lol layer
   
   function fast(factor, pat)
      if factor:eq(0) then
         return silence
      elseif factor:lt(0) then
         return rev(fast(-factor, pat))
      else
         return withTime(pat, function(t)
            return t * factor
         end, function(t)
            return t / factor
         end)
      end
   end
   register("fast :: Pattern Time -> Pattern a -> Pattern a", fast)
   
   local function slow(factor, pat)
      if factor:eq(0) then
         return silence
      else
         return fast(factor:reverse(), pat)
      end
   end
   register("slow :: Pattern Time -> Pattern a -> Pattern a", slow)
   
   -- rotL
   local function early(offset, pat)
      return withTime(pat, function(t)
         return t + offset
      end, function(t)
         return t - offset
      end)
   end
   register("early :: Time -> Pattern a -> Pattern a", early, false) -- HACK: why not patternify TIME??
   
   -- rotR
   local function late(offset, pat)
      return early(-offset, pat)
   end
   register("late :: Time -> Pattern a -> Pattern a", late, false)
   
   local function inside(np, f, pat)
      local function _inside(n)
         return fast(n, f(slow(n, pat)))
      end
      return innerJoin(fmap(np, _inside))
   end
   register("inside :: Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a", inside, false)
   
   local function outside(factor, f, pat)
      return inside(1 / factor, f, pat)
   end
   register("outside :: Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a", outside, false)
   
   local function ply(n, pat)
      pat = fmap(pat, function(x)
         return fast(n, pure(x))
      end)
      return squeezeJoin(pat)
   end
   register("ply :: Pattern Time -> Pattern a -> Pattern a", ply)
   
   local function fastgap(factor, pat)
      if factor:lte(0) then
         return silence
      end
      factor = factor:max(1)
      local mungeQuery = function(t)
         return t:sam() + ((t - t:sam()) * factor):min(1)
      end
      local eventSpanFunc = function(span)
         local b = span.start:sam() + (span.start - span.start:sam()) / factor
         local e = span.start:sam() + (span.stop - span.start:sam()) / factor
         return Span(b, e)
      end
      local query = function(span)
         local new_span = Span(mungeQuery(span.start), mungeQuery(span.stop))
         if new_span.start == new_span.start:nextSam() then
            return {}
         end
         local events = pat.query(new_span)
         for i = 1, #events do
            events[i] = events[i]:withSpan(eventSpanFunc)
         end
         return events
      end
      return splitQueries(Pattern(query))
   end
   register("fastgap :: Pattern Time -> Pattern a -> Pattern a", fastgap)
   
   function compress(b, e, pat)
      if b:gt(e) or e:gt(1) or b:gt(1) or b:lt(0) or e:lt(0) then
         return silence
      end
      local fasted = fastgap((e - b):reverse(), pat)
      return late(b, fasted)
   end
   register("compress :: Time -> Time -> Pattern a -> Pattern a", compress, false)
   
   function focus(b, e, pat)
      local fasted = fast((e - b):reverse(), pat)
      return late(b:cyclePos(), fasted)
   end
   register("focus :: Time -> Time -> Pattern a -> Pattern a", focus, false)
   
   local function zoom(s, e, pat)
      local dur = e - s
      local qf = function(span)
         return span:withCycle(function(t)
            return t * dur + s
         end)
      end
      local ef = function(span)
         return span:withCycle(function(t)
            return (t - s) / dur
         end)
      end
      return splitQueries(withEventSpan(withQuerySpan(pat, qf), ef))
   end
   register("zoom :: Time -> Time -> Pattern a -> Pattern a", zoom, false)
   
   local _run = function(n)
      local list = {}
      for i = 1, n do
         list[i] = i - 1
      end
      return fastcat(list)
   end
   
   local function run(n)
      return join(fmap(n, _run))
   end
   register("run :: Pattern Int -> Pattern Int", run, false)
   
   local _scan = function(n)
      local res = {}
      for i = 1, n do
         res[i] = run(pure(i))
      end
      return slowcat(res)
   end
   
   local function scan(n)
      return join(fmap(n, _scan))
   end
   register("scan :: Pattern Int -> Pattern Int", scan, false)
   
   local function segment(n, pat)
      return appLeft(fast(n, pure(id)), pat)
   end
   register("segment :: Pattern Time -> Pattern a -> Pattern a", segment)
   
   function range(mi, ma, pat)
      return pat * (ma - mi) + mi
   end
   register("range :: Pattern number -> Pattern number -> Pattern number -> Pattern a", range)
   
   local waveform = function(func)
      local query = function(span)
         return { Event(nil, span, func(span:midpoint())) }
      end
   
      return Pattern(query)
   end
   
   pattern.steady = function(value)
      return Pattern(function(span)
         return { Event(nil, span, value) }
      end)
   end
   local toBipolar = function(pat)
      return pat * 2 - 1
   end
   
   local fromBipolar = function(pat)
      return (pat + 1) / 2
   end
   
   -- stylua: ignore start
   local sine2 = waveform(function(t) return sin(t:asFloat() * pi * 2) end)
   local sine = fromBipolar(sine2)
   local cosine2 = late(1 / 4, sine2)
   local cosine = fromBipolar(cosine2)
   local square = waveform(function(t) return floor((t * 2) % 2) end)
   local square2 = toBipolar(square)
   local isaw = waveform(function(t) return -(t % 1) + 1 end)
   local isaw2 = toBipolar(isaw)
   local saw = waveform(function(t) return t % 1 end)
   local saw2 = toBipolar(saw)
   local tri = fastcat { isaw, saw }
   local tri2 = fastcat { isaw2, saw2 }
   local time = waveform(id)
   local rand = waveform(timeToRand)
   -- stylua: ignore end
   
   local _irand = function(i)
      return fmap(rand, function(x)
         return floor(x * i)
      end)
   end
   
   local irand = function(ipat)
      return innerJoin(fmap(ipat, _irand))
   end
   register("irand :: Pattern Num -> Pattern Num", irand)
   
   local _chooseWith = function(pat, vals)
      if #vals == 0 then
         return silence
      end
      return fmap(range(1, #vals + 1, pat), function(i)
         local key = min(max(floor(i), 0), #vals)
         return vals[key]
      end)
   end
   
   local chooseWith = function(pat, ...)
      return outerJoin(_chooseWith(pat, ...))
   end
   
   local chooseInWith = function(pat, vals)
      return innerJoin(_chooseWith(pat, vals))
   end
   
   local choose = function(vals)
      return chooseInWith(rand, vals)
   end
   
   local randcat = function(pats)
      return pattern.segment(1, choose(pats))
   end
   register("randcat :: [Pattern a] -> Pattern a", randcat, false)
   
   local function degradeByWith(prand, by, pat)
      if T(by) == "time" then
         by = by:asFloat()
      end
      local f = function(v)
         return v > by
      end
      return appLeft(
         fmap(pat, function(val)
            return function(_)
               return val
            end
         end),
         filterValues(prand, f)
      )
   end
   
   register("degradeByWith :: Pattern Double -> Double -> Pattern a -> Pattern a", degradeByWith)
   
   local function degradeBy(by, pat)
      return degradeByWith(rand, by, pat)
   end
   register("degradeBy :: Pattern Double -> Pattern a -> Pattern a", degradeBy)
   
   local function undegradeBy(by, pat)
      return degradeByWith(
         fmap(rand, function(r)
            return 1 - r
         end),
         by,
         pat
      )
   end
   register("undegradeBy :: Pattern Double -> Pattern a -> Pattern a", undegradeBy)
   
   local function degrade(pat)
      return degradeBy(0.5, pat)
   end
   register("degrade :: Pattern a -> Pattern a", degrade)
   
   local function undegrade(pat)
      return undegradeBy(0.5, pat)
   end
   register("undegrade :: Pattern a -> Pattern a", undegrade)
   
   local function sometimesBy(by, func, pat)
      local f = function()
         return overlay(degradeBy(by, pat), func(undegradeBy(1 - by, pat)))
      end
      return innerJoin(fmap(by, f))
   end
   register("sometimesBy :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a", sometimesBy)
   
   local function sometimes(func, pat)
      return sometimesBy(0.5, func, pat)
   end
   register("sometimes :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a", sometimes)
   
   local function struct(boolpat, pat)
      return op.keepif.Out(pat, boolpat)
   end
   register("struct :: [Pattern bool] -> Pattern a -> Pattern a", struct, false)
   
   local function mask(boolpat, pat)
      return op.keepif.In(pat, boolpat)
   end
   register("mask :: [Pattern bool] -> Pattern a -> Pattern a", mask, false)
   
   local function euclid(n, k, pat)
      return struct(bjork(n, k, 0), pat)
   end
   register("euclid :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a", euclid)
   
   local function euclidRot(n, k, rot, pat)
      return struct(bjork(n, k, rot), pat)
   end
   register("euclidRot :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a", euclidRot)
   
   function rev(pat)
      local query = function(span)
         local cycle = span.start:sam()
         local nextCycle = span.start:nextSam()
         local reflect = function(to_reflect)
            local reflected = to_reflect:withTime(function(t)
               return cycle + (nextCycle - t)
            end)
            local tmp = reflected.start
            reflected.start = reflected.stop
            reflected.stop = tmp
            return reflected
         end
         local events = pat.query(reflect(span))
         for i = 1, #events do
            events[i] = events[i]:withSpan(reflect)
         end
         return events
      end
      return Pattern(query)
   end
   register("rev :: Pattern a -> Pattern a", rev)
   
   local function iter(n, pat)
      local acc = {}
      for i = 1, n do
         acc[i] = early((i - 1) / n, pat)
      end
      return slowcat(acc)
   end
   register("iter :: Pattern Int -> Pattern a -> Pattern a", iter)
   
   local function reviter(n, pat)
      local acc = {}
      for i = 1, n do
         acc[i] = late((i - 1) / n, pat)
      end
      return slowcat(acc)
   end
   register("reviter :: Pattern Int -> Pattern a -> Pattern a", reviter)
   
   local function echoWith(times, tim, f, pat)
      local acc = {}
      for i = 0, times - 1 do
         acc[i] = f(pattern.late(tim * i, pat))
      end
      return stack(acc)
   end
   register("echoWith :: Pattern Int -> Pattern Int -> Pattern f -> Pattern a -> Pattern a", echoWith)
   
   local function when(test, f, pat)
      local query = function(span)
         local cycle_idx = span.start:sam()
         if test(cycle_idx) then
            return f(pat).query(span)
         else
            return pat.query(span)
         end
      end
      return splitQueries(Pattern(query))
   end
   register("when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a", when)
   
   local slowcatPrime = function(pats)
      local query = function(span)
         local index = span.start:sam():asFloat() % #pats + 1
         local pat = pats[index]
         return pat.query(span)
      end
      return splitQueries(Pattern(query))
   end
   
   local function every(n, f, pat)
      local acc = {}
      for i = 1, n do
         acc[i] = (i == 1) and f(pat) or pat
      end
      return slowcatPrime(acc)
   end
   -- nicer to write than f as ( -> ), just reify f
   -- register("every :: Pattern Int -> Pattern (a -> a) -> Pattern a -> Pattern a", every)
   register("every :: Pattern Int -> Pattern f -> Pattern a -> Pattern a", every)
   
   local function off(tp, f, pat)
      return overlay(f(late(tp, pat)), pat)
   end
   -- HACK:
   register("off :: Pattern Time -> Pattern b -> Pattern a -> Pattern a", off)
   
   local function scale(name, pat)
      return fmap(pat, getScale(name))
   end
   -- TODO: "Pattern String -> Pattern a -> Pattern a",
   register("scale :: String -> Pattern a -> Pattern a", scale, false)
   
   local function chain(pat, other)
      return fmap(pat, function(a)
         return function(b)
            if T(a) == "table" then
               a[#a + 1] = b
               return a
            end
            return { a, b }
         end
      end):appLeft(other)
   end
   register("chain :: Pattern ValueMap -> Pattern ValueMap -> Pattern ValueMap", chain, false)
   
   -- CONTROLS
   local function juxBy(n, f, pat)
      n = n / 2
      local left = pattern.pan(0.5) - n + pat
      local right = pattern.pan(0.5) + n + pat
      return overlay(left, f(right))
   end
   -- "juxBy :: Pattern Double -> (Pattern ValueMap -> Pattern ValueMap) -> Pattern ValueMap -> Pattern ValueMap",
   register("juxBy :: Pattern Double -> Pattern f -> Pattern ValueMap -> Pattern ValueMap", juxBy)
   
   local function striate(n, pat)
      local pats = {}
      for i = 1, n do
         pats[i] = pat .. { ["begin"] = (i - 1) / n, ["end"] = i / n }
      end
      return fastcat(pats)
   end
   register("striate :: Pattern Int -> Pattern ValueMap -> Pattern ValueMap", striate)
   
   local function chop(n, pat)
      local func = function(p)
         local acc = {}
         for i = 1, n do
            acc[i] = union({ begin = (i - 1) / n, ["end"] = i / n }, p)
         end
         return fastcat(acc)
      end
      return squeezeBind(pat, func)
   end
   register("chop :: Pattern Int -> Pattern ValueMap -> Pattern ValueMap", chop)
   
   local function slice(npat, ipat, opat)
      return innerBind(npat, function(n)
         return outerBind(ipat, function(i)
            return outerBind(opat, function(o)
               o = (type(o) == "table") and o or { s = o }
               if type(n) == "table" then
                  o["begin"] = n[i]
                  o["end"] = n[i + 1]
               else
                  o["begin"] = i / n
                  o["end"] = (i + 1) / n
               end
               return pure(o)
            end)
         end)
      end)
   end
   register("slice :: Pattern b -> Pattern b -> Pattern a -> Pattern a", slice, false)
   
   local function splice(npat, ipat, opat)
      return innerJoin(fmap(npat, function(n)
         local sliced = slice(pure(n), ipat, opat)
         return withEvent(sliced, function(event)
            return event:withValue(function(v)
               local new_attri = {
                  -- TODO: cps
                  speed = 1 / n / event.whole:duration():asFloat() * (v.speed or 1),
                  unit = "c",
               }
               return union(new_attri, v)
            end)
         end)
      end))
   end
   register("splice :: Pattern b -> Pattern b -> Pattern a -> Pattern a", splice, false)
   
   local function loopAt(factor, pat)
      pat = pat .. pattern.speed(factor:reverse():asFloat()) .. pattern.unit "c"
      return slow(factor, pat)
   end
   register("loopAt :: Pattern Time -> Pattern ValueMap -> Pattern ValueMap", loopAt)
   
   local function fit(pat)
      return withEvent(pat, function(event)
         return event:withValue(function(value)
            return union(value, {
               speed = event.whole:duration():reverse():asFloat(),
               unit = "c",
            })
         end)
      end)
   end
   register("fit :: Pattern ValueMap -> Pattern ValueMap", fit)
   
   -- TODO: clashes with the control name ... should there be control(2) ??
   local function legato(factor, pat)
      return withEventSpan(pat, function(span)
         return Span(span.start, (span.start + span:duration() * factor))
      end)
   end
   register("legato :: Pattern Time -> Pattern a -> Pattern a", legato)
   
   local gcd_reduce = function(tab)
      return reduce(function(acc, value)
         return acc:gcd(value)
      end, tab[1], tab)
   end
   
   local function drawLine(pat, chars)
      chars = chars or 60
      pat = reify(pat)
      local cycle = 0
      local pos = Time(0)
      local lines = { "" }
      local emptyLine = ""
      while #lines[1] < chars do
         local events = pat(cycle, cycle + 1)
         local events_with_onset = filter(function(event)
            return event:hasOnset()
         end, events)
         local durations = map(function(ev)
            return ev:duration()
         end, events_with_onset)
         local charFraction = gcd_reduce(durations)
         local totalSlots = charFraction:reverse()
         lines = map(function(line)
            return line .. "|"
         end, lines)
         emptyLine = emptyLine .. "|"
         for _ = 1, totalSlots:asFloat() do
            local start, stop = pos, pos + charFraction
            local matches = filter(function(event)
               return event.whole.start <= start and event.whole.stop >= stop
            end, events)
            local missingLines = #matches - #lines
            if missingLines > 0 then
               for _ = 1, missingLines do
                  lines = lines .. missingLines
               end
            end
            lines = map(function(line, index)
               local event = matches[index]
               if event ~= nil then
                  local isOnset = event.whole.start == start
                  local char = nil
                  if isOnset then
                     char = event.value
                  else
                     char = "-"
                  end
                  return line .. char
               end
               return line .. "."
            end, lines)
            emptyLine = emptyLine .. "."
            pos = pos + charFraction
         end
         cycle = cycle + 1
      end
      return tconcat(lines)
   end
   mt.drawLine = drawLine
   pattern.drawLine = drawLine
   
   ---CONTROLS
   local parseChord = theory.parseChord
   local genericParams, aliasParams = control.genericParams, control.aliasParams
   
   ---@param name string
   local create = function(name)
      local withVal, f
      if type(name) == "table" then
         withVal = function(xs)
            if type(xs) == "table" then
               local acc = {}
               for i, x in ipairs(xs) do
                  acc[name[i]] = x
               end
               return ValueMap(acc)
            else
               return ValueMap { [name] = xs }
            end
         end
         f = function(args)
            return reify(args):fmap(withVal)
         end
         name = name[1]
      else
         f = function(arg)
            return reify { [name] = arg }
         end
      end
      pattern[name] = f
      mt[name] = function(self, arg)
         return self .. f(arg)
      end
   end
   
   for _, param in ipairs(genericParams) do
      create(param)
      if aliasParams[param] ~= nil then
         local alias = aliasParams[param]
         if type(alias) == "table" then
            for _, al in ipairs(alias) do
               pattern[al] = pattern[param]
               mt[al] = mt[param]
            end
         else
            pattern[alias] = pattern[param]
            mt[alias] = mt[param]
         end
      end
   end
   
   pattern.note = function(pat, arp)
      local function chordToStack(thing)
         if type(thing) == "string" then
            if type(parseChord(thing)) == "table" then
               local notes = parseChord(thing)
               return arp and fastcat(notes) or stack(notes) -- arp function
            end
            return reify(thing)
         elseif T(thing) == "pattern" then
            return outerJoin(thing:fmap(function(chord)
               local notes = parseChord(chord)
               return arp and fastcat(notes) or stack(notes)
            end))
         else
            return reify(thing)
         end
      end
      local withVal = function(v)
         return ValueMap { note = v }
      end
      return fmap(chordToStack(pat), withVal)
   end
   
   ---@param d number
   ---@param s string | number
   ---@return Pattern
   local function cF(d, s)
      s = tonumber(s) and tonumber(s) or s
      local query = function(span)
         local val = span.controls[s]
         local pat = pure(val or d)
         return pat.query(span)
      end
      return Pattern(query)
   end
   pattern.cF = cF
   
   pattern.n = pattern.note
   mt.note = function(self, arg)
      return self .. pattern.note(arg)
   end
   mt.n = mt.note
   
   pattern.op = op
   pattern.id = id
   pattern.T = T
   pattern.pipe = ut.pipe
   pattern.dump = ut.dump
   pattern.t = TYPES
   pattern.mt = mt
   pattern.tri2 = tri2
   pattern.tri = tri
   pattern.saw2 = saw2
   pattern.saw = saw
   pattern.isaw = isaw
   pattern.isaw2 = isaw2
   pattern.square2 = square2
   pattern.square = square
   pattern.cosine = cosine
   pattern.cosine2 = cosine2
   pattern.sine = sine
   pattern.sine2 = sine2
   pattern.rand = rand
   pattern.time = time
   
   pattern.squeezeJoin = squeezeJoin
   
end

   local mt = pattern.mt
   
   local modal = {}
   modal.version = "modal dev-1"
   modal.url = "https://github.com/noearc/modal"
   
   local pairs = pairs
   
   modal.Clock = Clock
   
   for name, func in pairs(notation) do
      modal[name] = func
   end
   
   for name, func in pairs(theory) do
      modal[name] = func
   end
   
   for name, func in pairs(factory) do
      modal[name] = func
      mt[name] = ut.method_wrap(func)
   end
   
   for name, func in pairs(types) do
      modal[name] = func
   end
   
   for name, func in pairs(pattern) do
      modal[name] = func
   end
   
   setmetatable(modal, {
      __index = _G,
   })
   
   setmetatable(modal, {
      __call = function(t, override)
         for k, v in pairs(t) do
            if _G[k] ~= nil then
               local msg = "function " .. k .. " already exists in global scope."
               print("WARNING: " .. msg)
               if override then
                  _G[k] = v
                  print("WARNING: " .. msg .. " Overwritten.")
               end
            else
               _G[k] = v
            end
         end
      end,
   })
   
do
   local function repl()
      local host = "localhost"
      local port = 9000
      local maxi = notation.maxi(modal)
   
      local keywords = {}
      for i, _ in pairs(modal) do
         keywords[#keywords + 1] = i
      end
   
      if has_RL then
         RL.set_complete_list(keywords)
         RL.set_options { keeplines = 1000, histfile = "~/.synopsis_history" }
         RL.set_readline_name "modal"
      end
   
      local ok, c = pcall(socket.connect, host, port)
   
      local optf = {
         ["?"] = function()
            return [[
   :v  show _VERSION
   :t  get type for lib func (TODO: for expression)
   :q  quit repl ]]
         end,
         t = function(a)
            return tostring(modal.t[a])
         end,
         v = function()
            return modal._VERSION
         end,
         -- info = function(name)
         --    return dump(doc[name])
         -- end,
         q = function()
            if c then
               c:close()
            end
            os.exit()
         end,
      }
   
      -- TODO: see luaish, first run as lua with multiline? no ambiguiaty?>
      local eval = function(a)
         if a:sub(1, 1) == ":" then
            local name, param = a:match "(%a+)%s(%a*)"
            name = name and name or a:sub(2, #a)
            param = param and param or nil
            return optf[name](param)
         else
            local fn = modal.ut.dump(maxi(a))
            return fn
         end
      end
   
      local function readline(a)
         io.write(a)
         return io.read()
      end
   
      local read = has_RL and RL.readline or readline
   
      local line
      print "modal repl   :? for help"
      while true do
         line = read "> "
         if line == "exit" then
            if c then
               c:close()
            end
            break
         end
   
         if line ~= "" then
            local res = eval(line)
            if res then
               print(res)
            end
            if has_RL then
               RL.add_history(line)
               -- RL.save_history()
            end
            if c then
               c:send(line .. "\n")
            end
         end
      end
   
      c:close()
      os.exit()
   end
   modal.repl = repl
   
end

do
   local function server()
      local maxi = notation.maxi(modal)
      local log = ut.log
   
      local clock = modal.DefaultClock
      clock:start()
   
      local host = "*"
      local port = arg[1] or 9000
      local sock = assert(socket.bind(host, port))
      local i, p = sock:getsockname()
      assert(i, p)
   
      print("Waiting connection from repl on " .. i .. ":" .. p .. "...")
      local c = assert(sock:accept())
      c:settimeout(0)
   
      print "Connected"
   
      local eval = function(a)
         local ok, fn = pcall(maxi, a)
         if not ok then
            log.warn("syntax error: " .. fn)
         else
            print(fn)
         end
      end
   
      local l, e
   
      local listen = function()
         l, e = c:receive()
         if not e then
            eval(l)
         end
      end
   
      repeat
         coroutine.resume(clock.co, listen)
      until false
   end
   
   modal.server = server
   
end

modal.ut = ut
return modal
