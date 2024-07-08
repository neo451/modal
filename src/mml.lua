-- Copyright (c) 2014-2015 Andrew Abbott
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

-- Using A as a base note, these are how many
-- semitones/steps away a note on the same octave is.
local steps = {
   a = 0,
   b = 2,
   c = -9,
   d = -7,
   e = -5,
   f = -4,
   g = -2,
}

local REF_FREQ = 440 -- A4
local REF_OCTAVE = 4
local ROOT_MULT = 2 ^ (1 / 12) -- A constant: the twelfth root of two.

-- See http://www.phy.mtu.edu/~suits/NoteFreqCalcs.html
-- for information on calculating note frequencies.

local function calculateNoteFrequency(n)
   return REF_FREQ * (ROOT_MULT ^ n)
end

local function calculateNoteSteps(str)
   local note, sharp, octave = string.match(str, "(%a)(#?)(%d)")
   return (octave - REF_OCTAVE) * 12 + steps[note] + (sharp == "" and 0 or 1)
end

-- Calculates how long a note is in seconds given a note fraction
-- (quarter note = 4, half note = 2, etc.) and a tempo (in beats per minute).
local function calculateNoteTime(notefrac, bpm)
   return (240 / notefrac) / bpm
end

local function calculateNote(note, outputType)
   local steps = calculateNoteSteps(note)
   if outputType == "frequency" then
      return calculateNoteFrequency(steps)
   elseif outputType == "steps" then
      return steps
   elseif outputType == "multiplier" then
      return ROOT_MULT ^ steps
   end
end

--[[
	Phrases:
		Note: n[#][l]
			n is the note (a-g)
			a # or + makes the note sharp and a - makes it flat
			l is the length of the note
				4 is quarter note (1/4), 2 is half note (1/2), etc.
				Excluding this uses the default length, set with "l"

		Rest: r[l]
			l is the length of the rest, specified the same way as note length

		Commands:
			t[n] - set tempo to n
			o[n] - set octave to n
			l[n] - set default note length to n
			v[n] - set volume to v
			> - increment octave by one
			< - decrement octave by one
]]

-- Receives a string of MML and returns a player.

-- When resumed, the player yields with the note (output set by outputType),
-- the time in seconds the note is to be played and the volume it should be played at.
-- It also yields for rests, with nil as the note and for the volume.
-- When the player reaches the end of the song, it will raise an error which
-- will be caught by coroutine.resume.

-- outputType can be:
-- "steps", outputs the number of semitones away from A 440 the note is.
-- "frequency", outputs the frequency of the note.
-- "multiplier", outputs frequency/440.

local function newPlayer(str, outputType)
   return coroutine.create(function()
      local octave = 4
      local tempo = 60
      local notelength = 4
      local volume = 10

      local pos = 1

      repeat
         local c, args, newpos = string.match(string.sub(str, pos), "^([%a<>])(%A-)%s-()[%a<>]")

         if not c then -- Might be the last command in the string.
            c, args = string.match(string.sub(str, pos), "^([%a<>])(%A-)")
            newpos = 0
         end

         if not c then -- Probably bad syntax.
            error "Malformed MML"
         end

         pos = pos + (newpos - 1)

         if c == "o" then -- Set octave
            octave = tonumber(args)
         elseif c == "t" then -- Set tempo
            tempo = tonumber(args)
         elseif c == "v" then -- Set volume
            volume = tonumber(args)
         elseif c == "r" then -- Rest
            local delay
            if args ~= "" then
               delay = calculateNoteTime(tonumber(args), tempo)
            else
               delay = calculateNoteTime(notelength, tempo)
            end
            coroutine.yield(nil, delay, nil)
         elseif c == "l" then -- Set note length
            notelength = tonumber(args)
         elseif c == ">" then -- Increase octave
            octave = octave + 1
         elseif c == "<" then -- Decrease octave
            octave = octave - 1
         elseif c:find "[a-g]" then -- Play note
            local note
            local mod = string.match(args, "[+#-]")
            if mod then
               if mod == "#" or mod == "+" then
                  note = c .. "#" .. octave
               elseif mod == "-" then
                  note = c .. "-" .. octave
               end
            else
               note = c .. octave
            end

            local notetime
            local len = string.match(args, "%d+")
            if len then
               notetime = calculateNoteTime(tonumber(len), tempo)
            else
               notetime = calculateNoteTime(notelength, tempo)
            end

            -- Dotted notes
            if string.find(args, "%.") then
               notetime = notetime * 1.5
            end

            local output = calculateNote(note, outputType)
            coroutine.yield(output, notetime, volume)
         end
      until newpos == 0
      -- The coroutine deliberately raises an error when it
      -- finishes so coroutine.resume returns false as
      -- its first argument.
      error "Player finished"
   end)
end

local canon = "t110 l16 o5  a8f#g a8f#g a<ab>c#def#g f#8de f#8<f#gabagaf#ga"

local function mml(str)
   local tups = {}
   local player = newPlayer(str, "steps")
   while true do
      if coroutine.status(player) ~= "dead" then
         local _, n, time, vol = coroutine.resume(player)
         if type(n) ~= "string" then
            tups[#tups + 1] = time
            tups[#tups + 1] = n
         end
      else
         break
      end
   end
   return tups
end

require "modal"()

print(arrange(mml(canon)))

return mml
