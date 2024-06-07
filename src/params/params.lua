local genericParams = {
   {
      "s",
      -- {
      --    "s",
      --    "n",
      --    "gain",
      -- },
   },
   {
      -- {
      --    "bandf",
      --    "bandq",
      --    "bpenv",
      -- },
      "a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.",
   },
   {
      "toArg",
      "for internal sound routing",
   },
   {
      "from",
      "for internal sound routing",
   },
   {
      "to",
      "for internal sound routing",
   },
   {
      "accelerate",
      "a pattern of numbers that speed up (or slow down) samples while they play.",
   },
   {
      "amp",
      "like @gain@, but linear.",
   },
   {
      "attack",
      "a pattern of numbers to specify the attack time (in seconds) of an envelope applied to each sample.",
   },
   {
      "bandq",
      "a pattern of anumbers from 0 to 1. Sets the q-factor of the band-pass filter.",
   },
   {
      "begin",
      "a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. `0.25` to cut off the first quarter from each sample.",
   },
   {
      "legato",
      "controls the amount of overlap between two adjacent sounds",
   },
   {
      "clhatdecay",
      "",
   },
   {
      "crush",
      "bit crushing, a pattern of numbers from 1 (for drastic reduction in bit-depth) to 16 (for barely no reduction).",
   },
   {
      "coarse",
      "fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on.",
   },
   {
      "channel",
      "choose the channel the pattern is sent to in superdirt",
   },
   {
      "cut",
      "In the style of classic drum-machines, `cut` will stop a playing sample as soon as another samples with in same cutgroup is to be played. An example would be an open hi-hat followed by a closed one, essentially muting the open.",
   },
   {
      "cutoff",
      "a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter.",
   },
   {
      "cutoffegint",
      "",
   },
   {
      "decay",
      "",
   },
   {
      "delay",
      "a pattern of numbers from 0 to 1. Sets the level of the delay signal.",
   },
   {
      "delayfeedback",
      "a pattern of numbers from 0 to 1. Sets the amount of delay feedback.",
   },
   {
      "delaytime",
      "a pattern of numbers from 0 to 1. Sets the length of the delay.",
   },
   {
      "detune",
      "",
   },
   {
      "djf",
      "DJ filter, below 0.5 is low pass filter, above is high pass filter.",
   },
   {
      "dry",
      "when set to `1` will disable all reverb for this pattern. See `room` and `size` for more information about reverb.",
   },
   {
      "end",
      "the same as `begin`, but cuts the end off samples, shortening them; e.g. `0.75` to cut off the last quarter of each sample.",
   },
   {
      "fadeTime",
      "Used when using begin/end or chop/striate and friends, to change the fade out time of the 'grain' envelope.",
   },
   {
      "fadeInTime",
      "As with fadeTime, but controls the fade in time of the grain envelope. Not used if the grain begins at position 0 in the sample.",
   },
   {
      "freq",
      "",
   },
   {
      "gain",
      "a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder. For the linear equivalent, see @amp@.",
   },
   {
      "gate",
      "",
   },
   {
      "hatgrain",
      "",
   },
   {
      "hcutoff",
      "a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter. Also has alias @hpf@",
   },
   {
      "hold",
      "a pattern of numbers to specify the hold time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` and `release` are also specified.",
   },
   {
      "hresonance",
      "a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter. Has alias @hpq@",
   },
   {
      "lagogo",
      "",
   },
   {
      "lclap",
      "",
   },
   {
      "lclaves",
      "",
   },
   {
      "lclhat",
      "",
   },
   {
      "lcrash",
      "",
   },
   {
      "leslie",
      "",
   },
   {
      "lrate",
      "",
   },
   {
      "lsize",
      "",
   },
   {
      "lfo",
      "",
   },
   {
      "lfocutoffint",
      "",
   },
   {
      "lfodelay",
      "",
   },
   {
      "lfoint",
      "",
   },
   {
      "lfopitchint",
      "",
   },
   {
      "lfoshape",
      "",
   },
   {
      "lfosync",
      "",
   },
   {
      "lhitom",
      "",
   },
   {
      "lkick",
      "",
   },
   {
      "llotom",
      "",
   },
   {
      "lock",
      "A pattern of numbers. Specifies whether delaytime is calculated relative to cps. When set to 1, delaytime is a direct multiple of a cycle.",
   },
   {
      "loop",
      "loops the sample (from `begin` to `end`) the specified number of times.",
   },
   {
      "lophat",
      "",
   },
   {
      "lsnare",
      "",
   },
   {
      "metatune",
      "A pattern of numbers. Specifies whether the pitch of played samples should be tuned relative to their pitch metadata, if it exists. When set to 1, pitch metadata is applied. When set to 0, pitch metadata is ignored.",
   },
   {
      "n",
      "The note or sample number to choose for a synth or sampleset",
   },
   {
      "note",
      "The note or pitch to play a sound or synth with",
   },
   {
      "degree",
      "",
   },
   {
      "mtranspose",
      "",
   },
   {
      "ctranspose",
      "",
   },
   {
      "harmonic",
      "",
   },
   {
      "stepsPerOctave",
      "",
   },
   {
      "octaveR",
      "",
   },
   {
      "nudge",
      "Nudges events into the future by the specified number of seconds. Negative numbers work up to a point as well (due to internal latency)",
   },
   {
      "octave",
      "",
   },
   {
      "offset",
      "",
   },
   {
      "ophatdecay",
      "",
   },
   {
      "orbit",
      "a pattern of numbers. An `orbit` is a global parameter context for patterns. Patterns with the same orbit will share hardware output bus offset and global effects, e.g. reverb and delay. The maximum number of orbits is specified in the superdirt startup, numbers higher than maximum will wrap around.",
   },
   {
      "overgain",
      "",
   },
   {
      "overshape",
      "",
   },
   {
      "pan",
      "a pattern of numbers between 0 and 1, from left to right (assuming stereo), once round a circle (assuming multichannel)",
   },
   {
      "panspan",
      "a pattern of numbers between -inf and inf, which controls how much multichannel output is fanned out (negative is backwards ordering)",
   },
   {
      "pansplay",
      "a pattern of numbers between 0.0 and 1.0, which controls the multichannel spread range (multichannel only)",
   },
   {
      "panwidth",
      "a pattern of numbers between 0.0 and inf, which controls how much each channel is distributed over neighbours (multichannel only)",
   },
   {
      "panorient",
      "a pattern of numbers between -1.0 and 1.0, which controls the relative position of the centre pan in a pair of adjacent speakers { multichannel only }",
   },
   {
      "pitch1",
      "",
   },
   {
      "pitch2",
      "",
   },
   {
      "pitch3",
      "",
   },
   {
      "portamento",
      "",
   },
   {
      "rate",
      "used in SuperDirt softsynths as a control rate or 'speed'",
   },
   {
      "release",
      "a pattern of numbers to specify the release time (in seconds) of an envelope applied to each sample.",
   },
   {
      "resonance",
      "a pattern of numbers from 0 to 1. Specifies the resonance of the low-pass filter.",
   },
   {
      "room",
      "a pattern of numbers from 0 to 1. Sets the level of reverb.",
   },
   {
      "sagogo",
      "",
   },
   {
      "sclap",
      "",
   },
   {
      "sclaves",
      "",
   },
   {
      "scrash",
      "",
   },
   {
      "semitone",
      "",
   },
   {
      "shape",
      "wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion.",
   },
   {
      "size",
      "a pattern of numbers from 0 to 1. Sets the perceptual size (reverb time) of the `room` to be used in reverb.",
   },
   {
      "slide",
      "",
   },
   {
      "speed",
      "a pattern of numbers which changes the speed of sample playback, i.e. a cheap way of changing pitch. Negative values will play the sample backwards!",
   },
   {
      "squiz",
      "",
   },
   {
      "stutterdepth",
      "",
   },
   {
      "stuttertime",
      "",
   },
   {
      "sustain",
      "",
   },
   {
      "timescale",
      "time stretch amount",
   },
   {
      "timescalewin",
      "time stretch window size",
   },
   {
      "tomdecay",
      "",
   },
   {
      "unit",
      'used in conjunction with `speed`, accepts values of "r" (rate, default behavior), "c" (cycles), or "s" (seconds). Using `unit "c"` means `speed` will be interpreted in units of cycles, e.g. `speed "1"` means samples will be stretched to fill a cycle. Using `unit "s"` means the playback speed will be adjusted so that the duration is the number of seconds specified by `speed`.',
   },
   {
      "velocity",
      "",
   },
   {
      "vcfegint",
      "",
   },
   {
      "vcoegint",
      "",
   },
   {
      "voice",
      "",
   },
   {
      "vowel",
      "formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect.",
   },
   {
      "waveloss",
      "",
   },
   {
      "dur",
      "",
   },
   {
      "modwheel",
      "",
   },
   {
      "expression",
      "",
   },
   {
      "sustainpedal",
      "",
   },
   {
      "tremolodepth",
      "Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'",
   },
   {
      "tremolorate",
      "Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'",
   },
   {
      "phaserdepth",
      "Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'",
   },
   {
      "phaserrate",
      "Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'",
   },
   {
      "fshift",
      "frequency shifter",
   },
   {
      "fshiftnote",
      "frequency shifter",
   },
   {
      "fshiftphase",
      "frequency shifter",
   },
   {
      "triode",
      "tube distortion",
   },
   {
      "krush",
      "shape/bass enhancer",
   },
   {
      "kcutoff",
      "",
   },
   {
      "octer",
      "octaver effect",
   },
   {
      "octersub",
      "octaver effect",
   },
   {
      "octersubsub",
      "octaver effect",
   },
   {
      "ring",
      "ring modulation",
   },
   {
      "ringf",
      "ring modulation",
   },
   {
      "ringdf",
      "ring modulation",
   },
   {
      "distort",
      "noisy fuzzy distortion",
   },
   {
      "freeze",
      "Spectral freeze",
   },
   {
      "xsdelay",
      "",
   },
   {
      "tsdelay",
      "",
   },
   {
      "real",
      "Spectral conform",
   },
   {
      "imag",
      "",
   },
   {
      "enhance",
      "Spectral enhance",
   },
   {
      "partials",
      "",
   },
   {
      "comb",
      "Spectral comb",
   },
   {
      "smear",
      "Spectral smear",
   },
   {
      "scram",
      "Spectral scramble",
   },
   {
      "binshift",
      "Spectral binshift",
   },
   {
      "hbrick",
      "High pass sort of spectral filter",
   },
   {
      "lbrick",
      "Low pass sort of spectral filter",
   },
}
local aliasParams = {
   s = "sound",
   note = "up",
   attack = "att",
   bandf = "bpf",
   bandq = "bpq",
   clhatdecay = "chdecay",
   cutoff = "ctf",
   cutoffegint = "ctfg",
   delayfeedback = "delayfb",
   delayfeedback = "dfb",
   delaytime = "delayt",
   delaytime = "dt",
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
   cutoff = "lpf",
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
local reify, stack, fastcat
do
   local _obj_0 = require "modal.pattern"
   reify, stack, fastcat = _obj_0.reify, _obj_0.stack, _obj_0.fastcat
end

local pat = require "modal.pattern"
local fmap, outerJoin = pat.fmap, pat.outerJoin

local parseChord = require("modal.chords").parseChord

local P = {}

local create = function(name)
   local withVal
   if type(name) == "table" then
      withVal = function(xs)
         if type(xs) == "table" then
            local acc = {}
            for i, x in ipairs(xs) do
               acc[name[i]] = x
            end
            return acc
         else
            return { [name] = xs }
         end
      end
      P[name[1]] = function(args)
         return fmap(fastcat(args), withVal)
      end
   else
      withVal = function(v)
         return { [name] = v }
      end
      P[name] = function(args)
         return fmap(reify(args), withVal)
      end
   end
end

for i = 1, #genericParams do
   local name = genericParams[i]
   local param = name[1]
   create(param)
   if aliasParams[param] ~= nil then
      local alias = aliasParams[param]
      P[alias] = P[param]
   end
end

local notemt = {
   __add = function(self, other)
      return {
         note = self.note + other.note,
      }
   end,
}

P.note = function(args)
   local chordToStack = function(thing)
      return stack(parseChord(thing))
   end
   local withVal = function(v)
      return setmetatable({
         note = v,
      }, notemt)
   end
   -- return fmap((fmap(reify(args), chordToStack)):outerJoin(), withVal)
   return fmap(reify(args), withVal)
end

return P
