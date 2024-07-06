local genericParams = {
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
   "pan",
   "panorient",
   "portamento",
   "sagogo",
   "semitone",
   "speed",
   "sustain",
   "unit",
   "voice",
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

local aliasParams = {
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

local pattern = require "pattern"
local reify, stack = pattern.reify, pattern.stack
local ut = require "ut"
local T = ut.T
local theory = require "theory"
local parseChord = theory.parseChord

local params = {}
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
            return { [name[1]] = xs }
         end
      end

      params[name[1]] = function(args)
         return reify(args):fmap(withVal)
      end
   else
      withVal = function(v)
         return { [name] = v }
      end
      params[name] = function(args)
         return reify(args):fmap(withVal)
      end
   end
end

for _, param in ipairs(genericParams) do
   create(param)
   if aliasParams[param] ~= nil then
      local alias = aliasParams[param]
      if type(alias) == "table" then
         for _, al in ipairs(alias) do
            print(al)
            params[al] = params[param]
         end
      else
         params[alias] = params[param]
      end
   end
end

params.note = function(pat)
   local notemt = {
      __add = function(self, other)
         -- HACK:
         if type(other) ~= "table" then
            other = { note = other }
         end
         return { note = self.note + other.note }
      end,
   }

   local function chordToStack(thing)
      if type(thing) == "string" then
         if type(parseChord(thing)) == "table" then
            return stack(parseChord(thing))
         end
         return reify(thing)
      elseif T(thing) == "pattern" then
         return thing
            :fmap(function(chord)
               return stack(parseChord(chord))
            end)
            :outerJoin()
      else
         return reify(thing)
      end
   end
   local withVal = function(v)
      return setmetatable({ note = v }, notemt)
      -- return { note = v }
   end
   return chordToStack(pat):fmap(withVal)
end

params.n = params.note

return params
