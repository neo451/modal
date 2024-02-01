-- (require"fun")!
import concat from require "xi.utils"
-- major chords
major = { 0,4,7 }
aug = { 0,4,8 }
six = { 0,4,7,9 }
sixNine = { 0,4,7,9,14 }
major7 = { 0,4,7,11 }
major9 = { 0,4,7,11,14 }
add9 = { 0,4,7,14 }
major11 = { 0,4,7,11,14,17 }
add11 = { 0,4,7,17 }
major13 = { 0,4,7,11,14,21 }
add13 = { 0,4,7,21 }
-- dominant chords
dom7 = { 0,4,7,10 }
dom9 = { 0,4,7,14 }
dom11 = { 0,4,7,17 }
dom13 = { 0,4,7,21 }
sevenFlat5 = { 0,4,6,10 }
sevenSharp5 = { 0,4,8,10 }
sevenFlat9 = { 0,4,7,10,13 }
nine = { 0,4,7,10,14 }
eleven = { 0,4,7,10,14,17 }
thirteen = { 0,4,7,10,14,17,21 }
-- minor chords
minor = { 0,3,7 }
diminished = { 0,3,6 }
minorSharp5 = { 0,3,8 }
minor6 = { 0,3,7,9 }
minorSixNine = { 0,3,9,7,14 }
minor7flat5 = { 0,3,6,10 }
minor7 = { 0,3,7,10 }
minor7sharp5 = { 0,3,8,10 }
minor7flat9 = { 0,3,7,10,13 }
minor7sharp9 = { 0,3,7,10,15 }
diminished7 = { 0,3,6,9 }
minor9 = { 0,3,7,10,14 }
minor11 = { 0,3,7,10,14,17 }
minor13 = { 0,3,7,10,14,17,21 }
minorMajor7 = { 0,3,7,11 }
-- other chords
one = { 0 }
five = { 0,7 }
sus2 = { 0,2,7 }
sus4 = { 0,5,7 }
sevenSus2 = { 0,2,7,10 }
sevenSus4 = { 0,5,7,10 }
nineSus4 = { 0,5,7,10,14 }
-- questionable chords
sevenFlat10 = { 0,4,7,10,15 }
nineSharp5 = { 0,1,13 }
minor9sharp5 = { 0,1,14 }
sevenSharp5flat9 = { 0,4,8,10,13 }
minor7sharp5flat9 = { 0,3,8,10,13 }
elevenSharp = { 0,4,7,10,14,18 }
minor11sharp = { 0,3,7,10,14,18 }

chordTable =
  "major": major
  "maj": major
  "M": major
  "aug": aug
  "plus": aug
  "sharp5": aug
  "six": six
  "6": six
  "sixNine": sixNine
  "six9": sixNine
  "sixby9": sixNine
  "6by9": sixNine
  "major7": major7
  "maj7": major7
  "major9": major9
  "maj9": major9
  "add9": add9
  "major11": major11
  "maj11": major11
  "add11": add11
  "major13": major13
  "maj13": major13
  "add13": add13
  "dom7": dom7
  "dom9": dom9
  "dom11": dom11
  "dom13": dom13
  "sevenFlat5": sevenFlat5
  "7f5": sevenFlat5
  "sevenSharp5": sevenSharp5
  "7s5": sevenSharp5
  "sevenFlat9": sevenFlat9
  "7f9": sevenFlat9
  "nine": nine
  "eleven": eleven
  "11": eleven
  "thirteen": thirteen
  "13": thirteen
  "minor": minor
  "min": minor
  "m": minor
  "diminished": diminished
  "dim": diminished
  "minorSharp5": minorSharp5
  "msharp5": minorSharp5
  "mS5": minorSharp5
  "minor6": minor6
  "min6": minor6
  "m6": minor6
  "minorSixNine": minorSixNine
  "minor69": minorSixNine
  "min69": minorSixNine
  "minSixNine": minorSixNine
  "m69": minorSixNine
  "mSixNine": minorSixNine
  "m6by9": minorSixNine
  "minor7flat5": minor7flat5
  "minor7f5": minor7flat5
  "min7flat5": minor7flat5
  "min7f5": minor7flat5
  "m7flat5": minor7flat5
  "m7f5": minor7flat5
  "minor7": minor7
  "min7": minor7
  "m7": minor7
  "minor7sharp5": minor7sharp5
  "minor7s5": minor7sharp5
  "min7sharp5": minor7sharp5
  "min7s5": minor7sharp5
  "m7sharp5": minor7sharp5
  "m7s5": minor7sharp5
  "minor7flat9": minor7flat9
  "minor7f9": minor7flat9
  "min7flat9": minor7flat9
  "min7f9": minor7flat9
  "m7flat9": minor7flat9
  "m7f9": minor7flat9
  "minor7sharp9": minor7sharp9
  "minor7s9": minor7sharp9
  "min7sharp9": minor7sharp9
  "min7s9": minor7sharp9
  "m7sharp9": minor7sharp9
  "m7s9": minor7sharp9
  "diminished7": diminished7
  "dim7": diminished7
  "minor9": minor9
  "min9": minor9
  "m9": minor9
  "minor11": minor11
  "min11": minor11
  "m11": minor11
  "minor13": minor13
  "min13": minor13
  "m13": minor13
  "minorMajor7": minorMajor7
  "minMaj7": minorMajor7
  "mmaj7": minorMajor7
  "one": one
  "1": one
  "five": five
  "5": five
  "sus2": sus2
  "sus4": sus4
  "sevenSus2": sevenSus2
  "7sus2": sevenSus2
  "sevenSus4": sevenSus4
  "7sus4": sevenSus4
  "nineSus4": nineSus4
  "ninesus4": nineSus4
  "9sus4": nineSus4
  "sevenFlat10": sevenFlat10
  "7f10": sevenFlat10
  "nineSharp5": nineSharp5
  "9sharp5": nineSharp5
  "9s5": nineSharp5
  "minor9sharp5": minor9sharp5
  "minor9s5": minor9sharp5
  "min9sharp5": minor9sharp5
  "min9s5": minor9sharp5
  "m9sharp5": minor9sharp5
  "m9s5": minor9sharp5
  "sevenSharp5flat9": sevenSharp5flat9
  "7s5f9": sevenSharp5flat9
  "minor7sharp5flat9": minor7sharp5flat9
  "m7sharp5flat9": minor7sharp5flat9
  "elevenSharp": elevenSharp
  "11s": elevenSharp
  "minor11sharp": minor11sharp
  "m11sharp": minor11sharp
  "m11s": minor11sharp


-- TODO: num(float or frac) to note

import P, S, V, R, C, Ct, Cc from require("lpeg")

token = (id) -> Ct Cc(id) * C(V(id))
root = token"root"
note = token"note"
chord = token"chord"
chordname = token"chordname"
chordmods = token"chordmods"
notename = token"notename"
notemods = token"notemods"
range = token"range"
open = token"open"
drop = token"drop"
invert = token"invert"
offset = token "offset"
octave = token"octave"
number = token"number"
sep = V "sep"


grammar = {
  "chord"
  chord: note * sep ^ -1 * chordname ^ -1 * chordmods ^ -1
  note: notename * notemods ^ -1

  chordname: R("az","09") ^ 1
  -- chordmods: chordmod ^ 0
  chordmods: ( sep * (range + open + drop + invert) ) ^ 0
  notename: R"ag"
  notemods: offset ^-1 * octave ^-1
  offset: S"sfn"
  octave: R"05"
  range: number
  open: P"o"
  drop: P"d" * number
  invert: P"i" * number
  number: R"09"
  sep: P"'"
}

grammar = Ct C grammar

notes = {
  c:0
  d:2
  e:4
  f:5
  g:7
  a:9
  b:11
}

pconcat = (table1, pivot, table2) ->
  table.insert table1, pivot
  for elem in *table2
    table.insert table1, elem
  return table1

qsort = (table) ->
  if #table <= 1 then return table
  pivot = table[1]
  rest = [ elem for elem in *table[2,#table]]
  left = [ elem for elem in *rest when elem <= pivot ]
  right = [ elem for elem in *rest when elem > pivot ]
  pconcat (qsort left), pivot, (qsort right)

open = (chord) ->
  chord[1] = chord[1] - 12
  chord[3] = chord[3] - 12
  return chord

drop = (n, chord) ->
  chord = qsort chord
  index = #chord - (n - 1)
  chord[index] = chord[index] - 12
  return chord

invert = (n, chord) ->
  chord = qsort chord
  for i = 1, n
    index = i % #chord
    if index == 0 then index = #chord
    chord[index] = chord[index] + 12
  return chord

range = (n, chord) ->
  new_tones = {}
  n = tonumber(n)
  if #chord > n
    return [ tone for tone in *chord[1,n]]
  else
    for i = #chord + 1, n
      index = i % #chord
      octave = math.ceil(i / #chord) - 1
      if index == 0 then index = #chord
      new_tone = chord[index] + (12 * octave)
      table.insert new_tones, new_tone
    return concat chord, new_tones

parseChord = (chord) ->
  if type(chord) == "number" then return chord
  ast = grammar\match chord
  notename = notes[ast[2][3][2]]
  offset = 0
  octave = 5
  if ast[2][4] != nil
    mods = ast[2][4]
    for mod in *mods[3, #mods]
      if mod[1] == "offset"
        offset = switch mod[2]
          when "s" then 1
          when "f" then -1
          else 0
      if mod[1] == "octave" then octave = tonumber mod[2]
  rootnote = notename + offset + (octave - 5) * 12
  if ast[3][2] == "" then return rootnote
  chordtable = chordTable[ast[3][2]]
  chordtable = totable map ((x) -> x + rootnote), chordtable
  if ast[4][2] != "" then
    for mod in *ast[4][3, #ast[4]]
      if mod[1] == "open" then chordtable = open chordtable
      if mod[1] == "drop" then chordtable = drop mod[3][2], chordtable
      if mod[1] == "range" then chordtable = range mod[3][2], chordtable
      if mod[1] == "invert" then chordtable = invert mod[3][2], chordtable
  return chordtable

return { :parseChord }
