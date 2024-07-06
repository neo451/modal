local ut = require "ut"
local lpeg = require "lpeg"
local theory = {}
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

return theory
