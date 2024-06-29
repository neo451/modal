---@meta

---@class Fraction
---@field denomenator number
---@field numerator number
---@field reverse function
---@field cyclePos function
---@operator sub (Fraction) : Fraction

---@alias Time Fraction | number | Pattern
---@alias ValueMap table<string, any>
---@alias Int number | Pattern
---@alias State table
---@alias Event {value: any, }

---@param value any
---@return Pattern
pure = function(value) end

---stack up pats in polymeter way
---@param steps Int
---@param pats Pattern[]
---@return Pattern
polymeter = function(steps, pats) end

---stack up pats
---@param pats Pattern[]
---@return Pattern
stack = function(pats) end

---@param pats any[]
---@return Pattern
slowcat = function(pats) end

---Like slowcat, but the items are crammed into one cycle.
---fastcat("e5", "b4", "d5", "c5") // "e5 b4 d5 c5"
---@param pats any[]
---@return Pattern
fastcat = function(pats) end

---Like [slowcat](lua://slowcat), but each step has a length, relative to the whole.
---@param tups (Pattern | Time)[]
---@return Pattern
timecat = function(tups) end

---Allows to arrange multiple patterns together over multiple cycles.
---Takes a variable number of arrays with two elements specifying the number of cycles and the pattern to use.
---@param tups (Pattern | number)[]
---@return Pattern
arrange = function(tups) end

---generate ons amount of events evenly in steps, with an offset
---@param ons number
---@param steps number
---@param offset number
---@return table
bjork = function(ons, steps, offset) end

---@param factor Time
---@param pat any
---@return Pattern
fast = function(factor, pat) end

---@param factor Time
---@param pat any
---@return Pattern
slow = function(factor, pat) end

---@param factor Time
---@param pat any
---@return Pattern
early = function(factor, pat) end

---@param factor Time
---@param pat any
---@return Pattern
late = function(factor, pat) end

---@param np Time
---@param f fun(b: any): Pattern | Pattern
---@param pat Pattern
inside = function(np, f, pat) end

---@param np Time
---@param f Pattern | function
---@param pat Pattern
outside = function(np, f, pat) end

---@param name Scales
---@param pat Pattern
---@return Pattern
scale = function(name, pat) end

-- auto generated
---@param v string | number
---@return Pattern
n = function(v) end

---@param v string | number | Chords
---@return Pattern
note = function(v) end

---@alias vowels "a" | "e" | "i" | "o" | "u"
---@param v vowels | string
---@return Pattern
vowel = function(v) end
---@param v number
---@return Pattern
channel = function(v) end
---@param v number
---@return Pattern
cut = function(v) end
---@param v 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8
---@return Pattern
orbit = function(v) end
---@param v table
---@return Pattern
array = function(v) end

---@param v string | number
---@return Pattern
s = function(v) end

toArg = function(v) end
from = function(v) end
to = function(v) end

accelerate = function(v) end
amp = function(v) end
attack = function(v) end
bandf = function(v) end
bandq = function(v) end
begin = function(v) end
legato = function(v) end
clhatdecay = function(v) end
crush = function(v) end
coarse = function(v) end
cutoff = function(v) end
cutoffegint = function(v) end
decay = function(v) end
delay = function(v) end
delayfeedback = function(v) end
delaytime = function(v) end
detune = function(v) end
djf = function(v) end
dry = function(v) end
_end = function(v) end
fadeTime = function(v) end
fadeInTime = function(v) end
freq = function(v) end
gain = function(v) end
gate = function(v) end
hatgrain = function(v) end
hcutoff = function(v) end
hold = function(v) end
hresonance = function(v) end
lagogo = function(v) end
lclap = function(v) end
lclaves = function(v) end
lclhat = function(v) end
lcrash = function(v) end
leslie = function(v) end
lrate = function(v) end
lsize = function(v) end
lfo = function(v) end
lfocutoffint = function(v) end
lfodelay = function(v) end
lfoint = function(v) end
lfopitchint = function(v) end
lfoshape = function(v) end
lfosync = function(v) end
lhitom = function(v) end
lkick = function(v) end
llotom = function(v) end
lock = function(v) end
loop = function(v) end
lophat = function(v) end
lsnare = function(v) end
metatune = function(v) end
degree = function(v) end
mtranspose = function(v) end
ctranspose = function(v) end
harmonic = function(v) end
stepsPerOctave = function(v) end
octaveR = function(v) end
nudge = function(v) end
octave = function(v) end
offset = function(v) end
ophatdecay = function(v) end
overgain = function(v) end
overshape = function(v) end
pan = function(v) end
panspan = function(v) end
pansplay = function(v) end
panwidth = function(v) end
panorient = function(v) end
pitch1 = function(v) end
pitch2 = function(v) end
pitch3 = function(v) end
portamento = function(v) end
rate = function(v) end
release = function(v) end
resonance = function(v) end
room = function(v) end
sagogo = function(v) end
sclap = function(v) end
sclaves = function(v) end
scrash = function(v) end
semitone = function(v) end
shape = function(v) end
size = function(v) end
slide = function(v) end
speed = function(v) end
squiz = function(v) end
stutterdepth = function(v) end
stuttertime = function(v) end
sustain = function(v) end
timescale = function(v) end
timescalewin = function(v) end
tomdecay = function(v) end
unit = function(v) end
velocity = function(v) end
vcfegint = function(v) end
vcoegint = function(v) end
voice = function(v) end
waveloss = function(v) end
dur = function(v) end
modwheel = function(v) end
expression = function(v) end
sustainpedal = function(v) end
tremolodepth = function(v) end
tremolorate = function(v) end
phaserdepth = function(v) end
phaserrate = function(v) end
fshift = function(v) end
fshiftnote = function(v) end
fshiftphase = function(v) end
triode = function(v) end
krush = function(v) end
kcutoff = function(v) end
octer = function(v) end
octersub = function(v) end
octersubsub = function(v) end
ring = function(v) end
ringf = function(v) end
ringdf = function(v) end
distort = function(v) end
freeze = function(v) end
xsdelay = function(v) end
tsdelay = function(v) end
real = function(v) end
imag = function(v) end
enhance = function(v) end
partials = function(v) end
comb = function(v) end
smear = function(v) end
scram = function(v) end
binshift = function(v) end
hbrick = function(v) end
lbrick = function(v) end
midichan = function(v) end
control = function(v) end
ccn = function(v) end
ccv = function(v) end
polyTouch = function(v) end
midibend = function(v) end
miditouch = function(v) end
ctlNum = function(v) end
frameRate = function(v) end
frames = function(v) end
hours = function(v) end
midicmd = function(v) end
minutes = function(v) end
progNum = function(v) end
seconds = function(v) end
songPtr = function(v) end
uid = function(v) end
val = function(v) end
cps = function(v) end

---@class Pattern
---@operator add(Pattern) : Pattern
---@field query fun(st: State): Event[]
---@field scale fun(self: Pattern, name: Scales): Pattern
---@field fast fun(self: Pattern, factor: Time): Pattern
---@field slow fun(self: Pattern, factor: Time): Pattern
---@field vowel fun(self: Pattern, v:vowels | string): Pattern
---@field channel fun(self: Pattern, v:any): Pattern
---@field cut fun(self: Pattern, v:any): Pattern
---@field orbit fun(self: Pattern, v:any): Pattern
---@field array fun(self: Pattern, v:any): Pattern
---@field s fun(self: Pattern, v:any): Pattern
---@field n fun(self: Pattern, v:any): Pattern
---@field note fun(self: Pattern, v: string | number | Chords): Pattern
---@field toArg fun(self: Pattern, v:any): Pattern
---@field from fun(self: Pattern, v:any): Pattern
---@field to fun(self: Pattern, v:any): Pattern
---@field accelerate fun(self: Pattern, v:any): Pattern
---@field amp fun(self: Pattern, v:any): Pattern
---@field attack fun(self: Pattern, v:any): Pattern
---@field bandf fun(self: Pattern, v:any): Pattern
---@field bandq fun(self: Pattern, v:any): Pattern
---@field begin fun(self: Pattern, v:any): Pattern
---@field legato fun(self: Pattern, v:any): Pattern
---@field clhatdecay fun(self: Pattern, v:any): Pattern
---@field crush fun(self: Pattern, v:any): Pattern
---@field coarse fun(self: Pattern, v:any): Pattern
---@field cutoff fun(self: Pattern, v:any): Pattern
---@field cutoffegint fun(self: Pattern, v:any): Pattern
---@field decay fun(self: Pattern, v:any): Pattern
---@field delay fun(self: Pattern, v:any): Pattern
---@field delayfeedback fun(self: Pattern, v:any): Pattern
---@field delaytime fun(self: Pattern, v:any): Pattern
---@field detune fun(self: Pattern, v:any): Pattern
---@field djf fun(self: Pattern, v:any): Pattern
---@field dry fun(self: Pattern, v:any): Pattern
---@field _end fun(self: Pattern, v:any): Pattern
---@field fadeTime fun(self: Pattern, v:any): Pattern
---@field fadeInTime fun(self: Pattern, v:any): Pattern
---@field freq fun(self: Pattern, v:any): Pattern
---@field gain fun(self: Pattern, v:any): Pattern
---@field gate fun(self: Pattern, v:any): Pattern
---@field hatgrain fun(self: Pattern, v:any): Pattern
---@field hcutoff fun(self: Pattern, v:any): Pattern
---@field hold fun(self: Pattern, v:any): Pattern
---@field hresonance fun(self: Pattern, v:any): Pattern
---@field lagogo fun(self: Pattern, v:any): Pattern
---@field lclap fun(self: Pattern, v:any): Pattern
---@field lclaves fun(self: Pattern, v:any): Pattern
---@field lclhat fun(self: Pattern, v:any): Pattern
---@field lcrash fun(self: Pattern, v:any): Pattern
---@field leslie fun(self: Pattern, v:any): Pattern
---@field lrate fun(self: Pattern, v:any): Pattern
---@field lsize fun(self: Pattern, v:any): Pattern
---@field lfo fun(self: Pattern, v:any): Pattern
---@field lfocutoffint fun(self: Pattern, v:any): Pattern
---@field lfodelay fun(self: Pattern, v:any): Pattern
---@field lfoint fun(self: Pattern, v:any): Pattern
---@field lfopitchint fun(self: Pattern, v:any): Pattern
---@field lfoshape fun(self: Pattern, v:any): Pattern
---@field lfosync fun(self: Pattern, v:any): Pattern
---@field lhitom fun(self: Pattern, v:any): Pattern
---@field lkick fun(self: Pattern, v:any): Pattern
---@field llotom fun(self: Pattern, v:any): Pattern
---@field lock fun(self: Pattern, v:any): Pattern
---@field loop fun(self: Pattern, v:any): Pattern
---@field lophat fun(self: Pattern, v:any): Pattern
---@field lsnare fun(self: Pattern, v:any): Pattern
---@field metatune fun(self: Pattern, v:any): Pattern
---@field degree fun(self: Pattern, v:any): Pattern
---@field mtranspose fun(self: Pattern, v:any): Pattern
---@field ctranspose fun(self: Pattern, v:any): Pattern
---@field harmonic fun(self: Pattern, v:any): Pattern
---@field stepsPerOctave fun(self: Pattern, v:any): Pattern
---@field octaveR fun(self: Pattern, v:any): Pattern
---@field nudge fun(self: Pattern, v:any): Pattern
---@field octave fun(self: Pattern, v:any): Pattern
---@field offset fun(self: Pattern, v:any): Pattern
---@field ophatdecay fun(self: Pattern, v:any): Pattern
---@field overgain fun(self: Pattern, v:any): Pattern
---@field overshape fun(self: Pattern, v:any): Pattern
---@field pan fun(self: Pattern, v:any): Pattern
---@field panspan fun(self: Pattern, v:any): Pattern
---@field pansplay fun(self: Pattern, v:any): Pattern
---@field panwidth fun(self: Pattern, v:any): Pattern
---@field panorient fun(self: Pattern, v:any): Pattern
---@field pitch1 fun(self: Pattern, v:any): Pattern
---@field pitch2 fun(self: Pattern, v:any): Pattern
---@field pitch3 fun(self: Pattern, v:any): Pattern
---@field portamento fun(self: Pattern, v:any): Pattern
---@field rate fun(self: Pattern, v:any): Pattern
---@field release fun(self: Pattern, v:any): Pattern
---@field resonance fun(self: Pattern, v:any): Pattern
---@field room fun(self: Pattern, v:any): Pattern
---@field sagogo fun(self: Pattern, v:any): Pattern
---@field sclap fun(self: Pattern, v:any): Pattern
---@field sclaves fun(self: Pattern, v:any): Pattern
---@field scrash fun(self: Pattern, v:any): Pattern
---@field semitone fun(self: Pattern, v:any): Pattern
---@field shape fun(self: Pattern, v:any): Pattern
---@field size fun(self: Pattern, v:any): Pattern
---@field slide fun(self: Pattern, v:any): Pattern
---@field speed fun(self: Pattern, v:any): Pattern
---@field squiz fun(self: Pattern, v:any): Pattern
---@field stutterdepth fun(self: Pattern, v:any): Pattern
---@field stuttertime fun(self: Pattern, v:any): Pattern
---@field sustain fun(self: Pattern, v:any): Pattern
---@field timescale fun(self: Pattern, v:any): Pattern
---@field timescalewin fun(self: Pattern, v:any): Pattern
---@field tomdecay fun(self: Pattern, v:any): Pattern
---@field unit fun(self: Pattern, v:any): Pattern
---@field velocity fun(self: Pattern, v:any): Pattern
---@field vcfegint fun(self: Pattern, v:any): Pattern
---@field vcoegint fun(self: Pattern, v:any): Pattern
---@field voice fun(self: Pattern, v:any): Pattern
---@field waveloss fun(self: Pattern, v:any): Pattern
---@field dur fun(self: Pattern, v:any): Pattern
---@field modwheel fun(self: Pattern, v:any): Pattern
---@field expression fun(self: Pattern, v:any): Pattern
---@field sustainpedal fun(self: Pattern, v:any): Pattern
---@field tremolodepth fun(self: Pattern, v:any): Pattern
---@field tremolorate fun(self: Pattern, v:any): Pattern
---@field phaserdepth fun(self: Pattern, v:any): Pattern
---@field phaserrate fun(self: Pattern, v:any): Pattern
---@field fshift fun(self: Pattern, v:any): Pattern
---@field fshiftnote fun(self: Pattern, v:any): Pattern
---@field fshiftphase fun(self: Pattern, v:any): Pattern
---@field triode fun(self: Pattern, v:any): Pattern
---@field krush fun(self: Pattern, v:any): Pattern
---@field kcutoff fun(self: Pattern, v:any): Pattern
---@field octer fun(self: Pattern, v:any): Pattern
---@field octersub fun(self: Pattern, v:any): Pattern
---@field octersubsub fun(self: Pattern, v:any): Pattern
---@field ring fun(self: Pattern, v:any): Pattern
---@field ringf fun(self: Pattern, v:any): Pattern
---@field ringdf fun(self: Pattern, v:any): Pattern
---@field distort fun(self: Pattern, v:any): Pattern
---@field freeze fun(self: Pattern, v:any): Pattern
---@field xsdelay fun(self: Pattern, v:any): Pattern
---@field tsdelay fun(self: Pattern, v:any): Pattern
---@field real fun(self: Pattern, v:any): Pattern
---@field imag fun(self: Pattern, v:any): Pattern
---@field enhance fun(self: Pattern, v:any): Pattern
---@field partials fun(self: Pattern, v:any): Pattern
---@field comb fun(self: Pattern, v:any): Pattern
---@field smear fun(self: Pattern, v:any): Pattern
---@field scram fun(self: Pattern, v:any): Pattern
---@field binshift fun(self: Pattern, v:any): Pattern
---@field hbrick fun(self: Pattern, v:any): Pattern
---@field lbrick fun(self: Pattern, v:any): Pattern
---@field midichan fun(self: Pattern, v:any): Pattern
---@field control fun(self: Pattern, v:any): Pattern
---@field ccn fun(self: Pattern, v:any): Pattern
---@field ccv fun(self: Pattern, v:any): Pattern
---@field polyTouch fun(self: Pattern, v:any): Pattern
---@field midibend fun(self: Pattern, v:any): Pattern
---@field miditouch fun(self: Pattern, v:any): Pattern
---@field ctlNum fun(self: Pattern, v:any): Pattern
---@field frameRate fun(self: Pattern, v:any): Pattern
---@field frames fun(self: Pattern, v:any): Pattern
---@field hours fun(self: Pattern, v:any): Pattern
---@field midicmd fun(self: Pattern, v:any): Pattern
---@field minutes fun(self: Pattern, v:any): Pattern
---@field progNum fun(self: Pattern, v:any): Pattern
---@field seconds fun(self: Pattern, v:any): Pattern
---@field songPtr fun(self: Pattern, v:any): Pattern
---@field uid fun(self: Pattern, v:any): Pattern
---@field val fun(self: Pattern, v:any): Pattern
---@field cps fun(self: Pattern, v:any): Pattern
