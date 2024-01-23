require "moon.all"
-- five notes scales
minPent = { 0,3,5,7,10 }
majPent = { 0,2,4,7,9 }

--  another mode of major pentatonic
ritusen = { 0,2,5,7,9 }

-- another mode of major pentatonic
egyptian = { 0,2,5,7,10 }

--
kumai = { 0,2,3,7,9 }
hirajoshi = { 0,2,3,7,8 }
iwato = { 0,1,5,6,10 }
chinese = { 0,4,6,7,11 }
indian = { 0,4,5,7,10 }
pelog = { 0,1,3,7,8 }

--
prometheus = { 0,2,4,6,11 }
scriabin = { 0,1,4,7,9 }

-- han chinese pentatonic scales
gong = { 0,2,4,7,9 }
shang = { 0,2,5,7,10 }
jiao = { 0,3,5,8,10 }
zhi = { 0,2,5,7,9 }
yu = { 0,3,5,7,10 }

-- 6 note scales
whole = { 0,2,4,6,8,10 }
augmented = { 0,3,4,7,8,11 }
augmented2 = { 0,1,4,5,8,9 }

-- hexatonic modes with no tritone
hexMajor7 = { 0,2,4,7,9,11 }
hexDorian = { 0,2,3,5,7,10 }
hexPhrygian = { 0,1,3,5,8,10 }
hexSus = { 0,2,5,7,9,10 }
hexMajor6 = { 0,2,4,5,7,9 }
hexAeolian = { 0,3,5,7,8,10 }

-- 7 note scales
major = { 0,2,4,5,7,9,11 }
ionian = { 0,2,4,5,7,9,11 }
dorian = { 0,2,3,5,7,9,10 }
phrygian = { 0,1,3,5,7,8,10 }
lydian = { 0,2,4,6,7,9,11 }
mixolydian = { 0,2,4,5,7,9,10 }
aeolian = { 0,2,3,5,7,8,10 }
minor = { 0,2,3,5,7,8,10 }
locrian = { 0,1,3,5,6,8,10 }
harmonicMinor = { 0,2,3,5,7,8,11 }
harmonicMajor = { 0,2,4,5,7,8,11 }
melodicMinor = { 0,2,3,5,7,9,11 }
melodicMinorDesc = { 0,2,3,5,7,8,10 }
melodicMajor = { 0,2,4,5,7,8,10 }
bartok = melodicMajor
hindu = melodicMajor

-- raga modes
todi = { 0,1,3,6,7,8,11 }
purvi = { 0,1,4,6,7,8,11 }
marva = { 0,1,4,6,7,9,11 }
bhairav = { 0,1,4,5,7,8,11 }
ahirbhairav = { 0,1,4,5,7,9,10 }

--
superLocrian = { 0,1,3,4,6,8,10 }
romanianMinor = { 0,2,3,6,7,9,10 }
hungarianMinor = { 0,2,3,6,7,8,11 }
neapolitanMinor = { 0,1,3,5,7,8,11 }
enigmatic = { 0,1,4,6,8,10,11 }
spanish = { 0,1,4,5,7,8,10 }

-- modes of whole tones with added note ->
leadingWhole = { 0,2,4,6,8,10,11 }
lydianMinor = { 0,2,4,6,7,8,10 }
neapolitanMajor = { 0,1,3,5,7,9,11 }
locrianMajor = { 0,2,4,5,6,8,10 }

-- 8 note scales
diminished = { 0,1,3,4,6,7,9,10 }
diminished2 = { 0,2,3,5,6,8,9,11 }

-- modes of limited transposition
messiaen1 = whole
messiaen2 = diminished
messiaen3 = { 0, 2, 3, 4, 6, 7, 8, 10, 11 }
messiaen4 = { 0, 1, 2, 5, 6, 7, 8, 11 }
messiaen5 = { 0, 1, 5, 6, 7, 11 }
messiaen6 = { 0, 2, 4, 5, 6, 8, 10, 11 }
messiaen7 = { 0, 1, 2, 3, 5, 6, 7, 8, 9, 11 }

-- Arabic maqams taken from SuperCollider's Scale.sc
bayati = { 0, 1.5, 3, 5, 7, 8, 10 }
hijaz = { 0, 1, 4, 5, 7, 8.5, 10 }
sikah = { 0, 1.5, 3.5, 5.5, 7, 8.5, 10.5 }
rast = { 0, 2, 3.5, 5, 7, 9, 10.5 }
iraq = { 0, 1.5, 3.5, 5, 6.5, 8.5, 10.5 }
saba = { 0, 1.5, 3, 4, 6, 8, 10 }

-- 12 note scales
chromatic = { 0,1,2,3,4,5,6,7,8,9,10,11 }

scaleTable =
  "minPent": minPent
  "majPent": majPent
  "ritusen": ritusen
  "egyptian": egyptian
  "kumai": kumai
  "hirajoshi": hirajoshi
  "iwato": iwato
  "chinese": chinese
  "indian": indian
  "pelog": pelog
  "prometheus": prometheus
  "scriabin": scriabin
  "gong": gong
  "shang": shang
  "jiao": jiao
  "zhi": zhi
  "yu": yu
  "whole": whole
  "wholetone": whole
  "augmented": augmented
  "augmented2": augmented2
  "hexMajor7": hexMajor7
  "hexDorian": hexDorian
  "hexPhrygian": hexPhrygian
  "hexSus": hexSus
  "hexMajor6": hexMajor6
  "hexAeolian": hexAeolian
  "major": major
  "ionian": ionian
  "dorian": dorian
  "phrygian": phrygian
  "lydian": lydian
  "mixolydian": mixolydian
  "aeolian": aeolian
  "minor": minor
  "locrian": locrian
  "harmonicMinor": harmonicMinor
  "harmonicMajor": harmonicMajor
  "melodicMinor": melodicMinor
  "melodicMinorDesc": melodicMinorDesc
  "melodicMajor": melodicMajor
  "bartok": bartok
  "hindu": hindu
  "todi": todi
  "purvi": purvi
  "marva": marva
  "bhairav": bhairav
  "ahirbhairav": ahirbhairav
  "superLocrian": superLocrian
  "romanianMinor": romanianMinor
  "hungarianMinor": hungarianMinor
  "neapolitanMinor": neapolitanMinor
  "enigmatic": enigmatic
  "spanish": spanish
  "leadingWhole": leadingWhole
  "lydianMinor": lydianMinor
  "neapolitanMajor": neapolitanMajor
  "locrianMajor": locrianMajor
  "diminished": diminished
  "octatonic": diminished
  "diminished2": diminished2
  "octatonic2": diminished2
  "messiaen1": messiaen1
  "messiaen2": messiaen2
  "messiaen3": messiaen3
  "messiaen4": messiaen4
  "messiaen5": messiaen5
  "messiaen6": messiaen6
  "messiaen7": messiaen7
  "chromatic": chromatic
  "bayati": bayati
  "hijaz": hijaz
  "sikah": sikah
  "rast": rast
  "saba": saba
  "iraq": iraq

p scaleTable["yu"]
