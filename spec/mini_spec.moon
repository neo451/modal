import describe, it from require "busted"
import parse from require "modal.mini"
import mini, pure, silence, euclid, slowcat, fastcat, timecat, randcat, fast, slow, degrade, stack from require "modal.pattern"
C = require "modal.control"
local *

eval = (name) -> assert.same interpreter_targets[name], mini name

describe "Mini Interpreter for", ->
  describe "numbers", ->
    it "should pass", ->
      eval "45"
      eval "-2."
      eval "4.64"
      eval "-3"

  describe "words", ->
    it "should pass", ->
      eval "foo"

  describe "rests", ->
    it "should pass", ->
      eval "~"

  describe "sequences", ->
    it "should pass", ->
      eval "bd sd"
      eval "bd hh sd"
      eval "bd hh@2"
      eval "bd hh@3 sd@2"
      eval "bd! hh? ~ sd/2 cp*3"

  describe "euclidian rhythm", ->
    it "should pass", ->
      eval "bd(3,8,1)"

  describe "repeat", ->
    it "should pass", ->
      eval "hh!"
      eval "hh!!!"
      eval "bd! cp"
      eval "hh!4"
--       -- same "hh!4!!"

  describe "weight", ->
    it "should pass", ->
      eval "hh@2"

  describe "fast&slow", ->
    it "should pass", ->
      eval "bd*2"
      eval "bd/3"

  describe "range", ->
    it "should pass", ->
      eval "0 .. 9"

  describe "tail", ->
    it "should pass", ->
      eval "bd:3:2"

  describe "degrade", ->
    it "should pass", ->
      eval "hh?"
      eval "hh???"
--       -- eval "hh?4"
--       -- eval "hh?4??"
--       -- eval "hh??0.87"
--
  -- describe "hybrid mod", ->
  --   it "should pass", ->
  --     eval "hh!!??"
      -- eval "hh!/2?!"

  describe "random seq", ->
    it "should pass", ->
      eval "bd | sd cp"

  describe "polyrhythm", ->
    it "should pass", ->
      eval "[bd sd] hh"
      eval "bd sd . cp . hh*2"

  describe "stack", ->
      eval "[bd, sd]"

  describe "polymeter", ->
    it "should pass", ->
      eval "bd*<2 3 4>"
      eval "{bd sd hh cp hh}%4"

interpreter_targets = {
  -- numbers
  "45": pure 45
  "-2.": pure -2.0
  "4.64": pure 4.64
  "-3": pure -3
  -- word
  "foo": pure "foo"
  -- rest
  "~": silence!
  -- modifiers
  "bd*2": fast 2, pure"bd"
  "bd/3": slow 3, pure"bd"
  "hh?": degrade "hh"
  "hh???": degrade degrade degrade "hh"
  "hh!!??": degrade degrade fastcat "hh", "hh", "hh" -- TODO:
  "hh!": fastcat "hh", "hh"
  "hh!!!": fastcat "hh", "hh", "hh", "hh"
  "hh!4": fastcat "hh", "hh", "hh", "hh"
  "0 .. 9": fastcat [ i for i = 0, 9 ]
  "bd:3:2": pure({"bd", 3, 2})
  -- sequences
  "bd sd": fastcat "bd", "sd"
  "bd hh sd": fastcat "bd", "hh", "sd"
  "bd(3,8,1)": euclid 3, 8, 1, pure"bd"
  "hh@2": pure "hh"
  "bd hh@2": timecat { { 1, mini"bd" }, { 2, mini"hh" } }
  "bd hh@3 sd@2": timecat { { 1, mini"bd" }, { 3, mini"hh" }, { 2, mini"sd" } }
  "bd! cp": fastcat "bd", "bd", "cp"
  "bd! hh? ~ sd/2 cp*3": timecat {
    { 1, pure"bd" }
    { 1, pure"bd" }
    { 1, degrade(pure"hh") }
    { 1, silence! }
    { 1, slow(2, pure"sd") }
    { 1, fast(3, pure"cp") }
  }
  "bd | sd cp": randcat(pure"bd", fastcat("sd", "cp"))
  "bd sd . cp . hh*2": fastcat(fastcat("bd", "sd"), "cp", fast(2, mini"hh"))
  "[bd, sd]": stack "bd", "sd"
  "[bd sd] hh": fastcat (fastcat "bd", "sd"), "hh"
  "{bd sd hh cp hh}%4": fastcat("bd", "sd", "hh", "cp")
  "bd*<2 3 4>": slowcat fast(2, pure"bd"), fast(3, pure"bd"), fast(4, pure"bd")
}
