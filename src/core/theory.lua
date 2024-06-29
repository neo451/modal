local ut = require "modal.utils"
local lpeg = require "lpeg"
local theory = {}
local P, S, V, R, C, Ct, Cc = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct, lpeg.Cc

local concat, map = ut.concat, ut.map
local qsort = ut.quicksort

local major = { 0, 4, 7 }
local aug = { 0, 4, 8 }
local six = { 0, 4, 7, 9 }
local sixNine = { 0, 4, 7, 9, 14 }
local major7 = { 0, 4, 7, 11 }
local major9 = { 0, 4, 7, 11, 14 }
local add9 = { 0, 4, 7, 14 }
local major11 = { 0, 4, 7, 11, 14, 17 }
local add11 = { 0, 4, 7, 17 }
local major13 = { 0, 4, 7, 11, 14, 21 }
local add13 = { 0, 4, 7, 21 }
local dom7 = { 0, 4, 7, 10 }
local dom9 = { 0, 4, 7, 14 }
local dom11 = { 0, 4, 7, 17 }
local dom13 = { 0, 4, 7, 21 }
local sevenFlat5 = { 0, 4, 6, 10 }
local sevenSharp5 = { 0, 4, 8, 10 }
local sevenFlat9 = { 0, 4, 7, 10, 13 }
local nine = { 0, 4, 7, 10, 14 }
local eleven = { 0, 4, 7, 10, 14, 17 }
local thirteen = { 0, 4, 7, 10, 14, 17, 21 }
local minor = { 0, 3, 7 }
local diminished = { 0, 3, 6 }
local minorSharp5 = { 0, 3, 8 }
local minor6 = { 0, 3, 7, 9 }
local minorSixNine = { 0, 3, 9, 7, 14 }
local minor7flat5 = { 0, 3, 6, 10 }
local minor7 = { 0, 3, 7, 10 }
local minor7sharp5 = { 0, 3, 8, 10 }
local minor7flat9 = { 0, 3, 7, 10, 13 }
local minor7sharp9 = { 0, 3, 7, 10, 15 }
local diminished7 = { 0, 3, 6, 9 }
local minor9 = { 0, 3, 7, 10, 14 }
local minor11 = { 0, 3, 7, 10, 14, 17 }
local minor13 = { 0, 3, 7, 10, 14, 17, 21 }
local minorMajor7 = { 0, 3, 7, 11 }
local one = { 0 }
local five = { 0, 7 }
local sus2 = { 0, 2, 7 }
local sus4 = { 0, 5, 7 }
local sevenSus2 = { 0, 2, 7, 10 }
local sevenSus4 = { 0, 5, 7, 10 }
local nineSus4 = { 0, 5, 7, 10, 14 }
local sevenFlat10 = { 0, 4, 7, 10, 15 }
local nineSharp5 = { 0, 1, 13 }
local minor9sharp5 = { 0, 1, 14 }
local sevenSharp5flat9 = { 0, 4, 8, 10, 13 }
local minor7sharp5flat9 = { 0, 3, 8, 10, 13 }
local elevenSharp = { 0, 4, 7, 10, 14, 18 }
local minor11sharp = { 0, 3, 7, 10, 14, 18 }

---@enum (key) Chords
local chordTable = {
   ["major"] = major,
   ["maj"] = major,
   ["M"] = major,
   ["aug"] = aug,
   ["plus"] = aug,
   ["sharp5"] = aug,
   ["six"] = six,
   ["6"] = six,
   ["sixNine"] = sixNine,
   ["six9"] = sixNine,
   ["sixby9"] = sixNine,
   ["6by9"] = sixNine,
   ["major7"] = major7,
   ["maj7"] = major7,
   ["major9"] = major9,
   ["maj9"] = major9,
   ["add9"] = add9,
   ["major11"] = major11,
   ["maj11"] = major11,
   ["add11"] = add11,
   ["major13"] = major13,
   ["maj13"] = major13,
   ["add13"] = add13,
   ["dom7"] = dom7,
   ["dom9"] = dom9,
   ["dom11"] = dom11,
   ["dom13"] = dom13,
   ["sevenFlat5"] = sevenFlat5,
   ["7f5"] = sevenFlat5,
   ["sevenSharp5"] = sevenSharp5,
   ["7s5"] = sevenSharp5,
   ["sevenFlat9"] = sevenFlat9,
   ["7f9"] = sevenFlat9,
   ["nine"] = nine,
   ["eleven"] = eleven,
   ["11"] = eleven,
   ["thirteen"] = thirteen,
   ["13"] = thirteen,
   ["minor"] = minor,
   ["min"] = minor,
   ["m"] = minor,
   ["diminished"] = diminished,
   ["dim"] = diminished,
   ["minorSharp5"] = minorSharp5,
   ["msharp5"] = minorSharp5,
   ["mS5"] = minorSharp5,
   ["minor6"] = minor6,
   ["min6"] = minor6,
   ["m6"] = minor6,
   ["minorSixNine"] = minorSixNine,
   ["minor69"] = minorSixNine,
   ["min69"] = minorSixNine,
   ["minSixNine"] = minorSixNine,
   ["m69"] = minorSixNine,
   ["mSixNine"] = minorSixNine,
   ["m6by9"] = minorSixNine,
   ["minor7flat5"] = minor7flat5,
   ["minor7f5"] = minor7flat5,
   ["min7flat5"] = minor7flat5,
   ["min7f5"] = minor7flat5,
   ["m7flat5"] = minor7flat5,
   ["m7f5"] = minor7flat5,
   ["minor7"] = minor7,
   ["min7"] = minor7,
   ["m7"] = minor7,
   ["minor7sharp5"] = minor7sharp5,
   ["minor7s5"] = minor7sharp5,
   ["min7sharp5"] = minor7sharp5,
   ["min7s5"] = minor7sharp5,
   ["m7sharp5"] = minor7sharp5,
   ["m7s5"] = minor7sharp5,
   ["minor7flat9"] = minor7flat9,
   ["minor7f9"] = minor7flat9,
   ["min7flat9"] = minor7flat9,
   ["min7f9"] = minor7flat9,
   ["m7flat9"] = minor7flat9,
   ["m7f9"] = minor7flat9,
   ["minor7sharp9"] = minor7sharp9,
   ["minor7s9"] = minor7sharp9,
   ["min7sharp9"] = minor7sharp9,
   ["min7s9"] = minor7sharp9,
   ["m7sharp9"] = minor7sharp9,
   ["m7s9"] = minor7sharp9,
   ["diminished7"] = diminished7,
   ["dim7"] = diminished7,
   ["minor9"] = minor9,
   ["min9"] = minor9,
   ["m9"] = minor9,
   ["minor11"] = minor11,
   ["min11"] = minor11,
   ["m11"] = minor11,
   ["minor13"] = minor13,
   ["min13"] = minor13,
   ["m13"] = minor13,
   ["minorMajor7"] = minorMajor7,
   ["minMaj7"] = minorMajor7,
   ["mmaj7"] = minorMajor7,
   ["one"] = one,
   ["1"] = one,
   ["five"] = five,
   ["5"] = five,
   ["sus2"] = sus2,
   ["sus4"] = sus4,
   ["sevenSus2"] = sevenSus2,
   ["7sus2"] = sevenSus2,
   ["sevenSus4"] = sevenSus4,
   ["7sus4"] = sevenSus4,
   ["nineSus4"] = nineSus4,
   ["ninesus4"] = nineSus4,
   ["9sus4"] = nineSus4,
   ["sevenFlat10"] = sevenFlat10,
   ["7f10"] = sevenFlat10,
   ["nineSharp5"] = nineSharp5,
   ["9sharp5"] = nineSharp5,
   ["9s5"] = nineSharp5,
   ["minor9sharp5"] = minor9sharp5,
   ["minor9s5"] = minor9sharp5,
   ["min9sharp5"] = minor9sharp5,
   ["min9s5"] = minor9sharp5,
   ["m9sharp5"] = minor9sharp5,
   ["m9s5"] = minor9sharp5,
   ["sevenSharp5flat9"] = sevenSharp5flat9,
   ["7s5f9"] = sevenSharp5flat9,
   ["minor7sharp5flat9"] = minor7sharp5flat9,
   ["m7sharp5flat9"] = minor7sharp5flat9,
   ["elevenSharp"] = elevenSharp,
   ["11s"] = elevenSharp,
   ["minor11sharp"] = minor11sharp,
   ["m11sharp"] = minor11sharp,
   ["m11s"] = minor11sharp,
}

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

local minPent = { 0, 3, 5, 7, 10 }
local majPent = { 0, 2, 4, 7, 9 }
local ritusen = { 0, 2, 5, 7, 9 }
local egyptian = { 0, 2, 5, 7, 10 }
local kumai = { 0, 2, 3, 7, 9 }
local hirajoshi = { 0, 2, 3, 7, 8 }
local iwato = { 0, 1, 5, 6, 10 }
local chinese = { 0, 4, 6, 7, 11 }
local indian = { 0, 4, 5, 7, 10 }
local pelog = { 0, 1, 3, 7, 8 }
local prometheus = { 0, 2, 4, 6, 11 }
local scriabin = { 0, 1, 4, 7, 9 }
local gong = { 0, 2, 4, 7, 9 }
local shang = { 0, 2, 5, 7, 10 }
local jiao = { 0, 3, 5, 8, 10 }
local zhi = { 0, 2, 5, 7, 9 }
local yu = { 0, 3, 5, 7, 10 }
local whole = { 0, 2, 4, 6, 8, 10 }
local augmented = { 0, 3, 4, 7, 8, 11 }
local augmented2 = { 0, 1, 4, 5, 8, 9 }
local hexMajor7 = { 0, 2, 4, 7, 9, 11 }
local hexDorian = { 0, 2, 3, 5, 7, 10 }
local hexPhrygian = { 0, 1, 3, 5, 8, 10 }
local hexSus = { 0, 2, 5, 7, 9, 10 }
local hexMajor6 = { 0, 2, 4, 5, 7, 9 }
local hexAeolian = { 0, 3, 5, 7, 8, 10 }
local major = { 0, 2, 4, 5, 7, 9, 11 }
local ionian = { 0, 2, 4, 5, 7, 9, 11 }
local dorian = { 0, 2, 3, 5, 7, 9, 10 }
local phrygian = { 0, 1, 3, 5, 7, 8, 10 }
local lydian = { 0, 2, 4, 6, 7, 9, 11 }
local mixolydian = { 0, 2, 4, 5, 7, 9, 10 }
local aeolian = { 0, 2, 3, 5, 7, 8, 10 }
local minor = { 0, 2, 3, 5, 7, 8, 10 }
local locrian = { 0, 1, 3, 5, 6, 8, 10 }
local harmonicMinor = { 0, 2, 3, 5, 7, 8, 11 }
local harmonicMajor = { 0, 2, 4, 5, 7, 8, 11 }
local melodicMinor = { 0, 2, 3, 5, 7, 9, 11 }
local melodicMinorDesc = { 0, 2, 3, 5, 7, 8, 10 }
local melodicMajor = { 0, 2, 4, 5, 7, 8, 10 }
local bartok = melodicMajor
local hindu = melodicMajor
local todi = { 0, 1, 3, 6, 7, 8, 11 }
local purvi = { 0, 1, 4, 6, 7, 8, 11 }
local marva = { 0, 1, 4, 6, 7, 9, 11 }
local bhairav = { 0, 1, 4, 5, 7, 8, 11 }
local ahirbhairav = { 0, 1, 4, 5, 7, 9, 10 }
local superLocrian = { 0, 1, 3, 4, 6, 8, 10 }
local romanianMinor = { 0, 2, 3, 6, 7, 9, 10 }
local hungarianMinor = { 0, 2, 3, 6, 7, 8, 11 }
local neapolitanMinor = { 0, 1, 3, 5, 7, 8, 11 }
local enigmatic = { 0, 1, 4, 6, 8, 10, 11 }
local spanish = { 0, 1, 4, 5, 7, 8, 10 }
local leadingWhole = { 0, 2, 4, 6, 8, 10, 11 }
local lydianMinor = { 0, 2, 4, 6, 7, 8, 10 }
local neapolitanMajor = { 0, 1, 3, 5, 7, 9, 11 }
local locrianMajor = { 0, 2, 4, 5, 6, 8, 10 }
local diminished = { 0, 1, 3, 4, 6, 7, 9, 10 }
local diminished2 = { 0, 2, 3, 5, 6, 8, 9, 11 }
local messiaen1 = whole
local messiaen2 = diminished
local messiaen3 = { 0, 2, 3, 4, 6, 7, 8, 10, 11 }
local messiaen4 = { 0, 1, 2, 5, 6, 7, 8, 11 }
local messiaen5 = { 0, 1, 5, 6, 7, 11 }
local messiaen6 = { 0, 2, 4, 5, 6, 8, 10, 11 }
local messiaen7 = { 0, 1, 2, 3, 5, 6, 7, 8, 9, 11 }
local bayati = { 0, 1.5, 3, 5, 7, 8, 10 }
local hijaz = { 0, 1, 4, 5, 7, 8.5, 10 }
local sikah = { 0, 1.5, 3.5, 5.5, 7, 8.5, 10.5 }
local rast = { 0, 2, 3.5, 5, 7, 9, 10.5 }
local iraq = { 0, 1.5, 3.5, 5, 6.5, 8.5, 10.5 }
local saba = { 0, 1.5, 3, 4, 6, 8, 10 }
local chromatic = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 }

---@enum (key) Scales
local scaleTable = {
   ["minPent"] = minPent,
   ["majPent"] = majPent,
   ["ritusen"] = ritusen,
   ["egyptian"] = egyptian,
   ["kumai"] = kumai,
   ["hirajoshi"] = hirajoshi,
   ["iwato"] = iwato,
   ["chinese"] = chinese,
   ["indian"] = indian,
   ["pelog"] = pelog,
   ["prometheus"] = prometheus,
   ["scriabin"] = scriabin,
   ["gong"] = gong,
   ["shang"] = shang,
   ["jiao"] = jiao,
   ["zhi"] = zhi,
   ["yu"] = yu,
   ["whole"] = whole,
   ["wholetone"] = whole,
   ["augmented"] = augmented,
   ["augmented2"] = augmented2,
   ["hexMajor7"] = hexMajor7,
   ["hexDorian"] = hexDorian,
   ["hexPhrygian"] = hexPhrygian,
   ["hexSus"] = hexSus,
   ["hexMajor6"] = hexMajor6,
   ["hexAeolian"] = hexAeolian,
   ["major"] = major,
   ["ionian"] = ionian,
   ["dorian"] = dorian,
   ["phrygian"] = phrygian,
   ["lydian"] = lydian,
   ["mixolydian"] = mixolydian,
   ["aeolian"] = aeolian,
   ["minor"] = minor,
   ["locrian"] = locrian,
   ["harmonicMinor"] = harmonicMinor,
   ["harmonicMajor"] = harmonicMajor,
   ["melodicMinor"] = melodicMinor,
   ["melodicMinorDesc"] = melodicMinorDesc,
   ["melodicMajor"] = melodicMajor,
   ["bartok"] = bartok,
   ["hindu"] = hindu,
   ["todi"] = todi,
   ["purvi"] = purvi,
   ["marva"] = marva,
   ["bhairav"] = bhairav,
   ["ahirbhairav"] = ahirbhairav,
   ["superLocrian"] = superLocrian,
   ["romanianMinor"] = romanianMinor,
   ["hungarianMinor"] = hungarianMinor,
   ["neapolitanMinor"] = neapolitanMinor,
   ["enigmatic"] = enigmatic,
   ["spanish"] = spanish,
   ["leadingWhole"] = leadingWhole,
   ["lydianMinor"] = lydianMinor,
   ["neapolitanMajor"] = neapolitanMajor,
   ["locrianMajor"] = locrianMajor,
   ["diminished"] = diminished,
   ["octatonic"] = diminished,
   ["diminished2"] = diminished2,
   ["octatonic2"] = diminished2,
   ["messiaen1"] = messiaen1,
   ["messiaen2"] = messiaen2,
   ["messiaen3"] = messiaen3,
   ["messiaen4"] = messiaen4,
   ["messiaen5"] = messiaen5,
   ["messiaen6"] = messiaen6,
   ["messiaen7"] = messiaen7,
   ["chromatic"] = chromatic,
   ["bayati"] = bayati,
   ["hijaz"] = hijaz,
   ["sikah"] = sikah,
   ["rast"] = rast,
   ["saba"] = saba,
   ["iraq"] = iraq,
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
