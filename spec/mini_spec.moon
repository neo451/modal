import Span from require "xi.types"
import visit from require "xi.mini.visitor"
import mini, pure, silence, slowcat, fastcat, timecat, randcat, fast, slow, degrade, stack from require "xi.pattern"

same = (name) -> assert.same visitor_targets[name], visit name
eval = (name) -> assert.same interpreter_targets[name], mini name

describe "Mini Parser for", ->
  describe "numbers", ->
    it "should pass", ->
      same "45"
      same "-2."
      same "4.64"
      same "-3"

  describe "words", ->
    it "should pass", ->
      same "foo"
      same "Bar:2"

  describe "rests", ->
    it "should pass", ->
      same "~"

  describe "fast&slow", ->
    it "should pass", ->
      same "bd*2"
      same "bd/3"

  describe "degrade", ->
    it "should pass", ->
      same "hh?"
      same "hh???"
      same "hh?4"
      same "hh?4??"
      same "hh??0.87"

  describe "repeat", ->
    it "should pass", ->
      same "hh!"
      same "hh!!!"
      same "hh!4"
      same "hh!4!!"

  describe "weight", ->
    it "should pass", ->
      same "hh@2"
      same "bd _ _ sd"

  describe "hybrid mod", ->
    it "should pass", ->
      same "hh!!??!"
      -- TODO:more complex case

  describe "sequence", ->
    it "should pass", ->
      same "bd sd"
      same "bd hh sd"
      same "bd! hh? ~ sd/2 cp*3"

  describe "polymeter", ->
    it "should pass", ->
      same "bd*<2 3 4>"
      same "{bd sd hh cp hh}%4"

  describe "euclidian rhythm", ->
    it "should pass", ->
      same "1(3,8)"

  describe "polyrhythm", ->
    it "should pass", ->
      same "[bd sd] hh"
      same "bd sd . cp . hh*2"
      same "[bd, sd]"

  describe "random seq", ->
    it "should pass", ->
      same "bd | sd cp"

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
      eval "Bar:2"

  describe "rests", ->
    it "should pass", ->
      eval "~"

  describe "sequences", ->
    it "should pass", ->
      eval "bd sd"
      eval "bd hh sd"
      eval "bd hh@2"
      -- eval "bd hh@3 sd@2"
      eval "bd! hh? ~ sd/2 cp*3"

  describe "repeat", ->
    it "should pass", ->
      eval "hh!"
      eval "hh!!!"
      eval "bd! cp"
      -- same "hh!4"
      -- same "hh!4!!"

  describe "weight", ->
    it "should pass", ->
      eval "hh@2"

  describe "fast&slow", ->
    it "should pass", ->
      eval "bd*2"
      eval "bd/3"

  describe "degrade", ->
    it "should pass", ->
      eval "hh?"
      -- eval "hh???"
      -- eval "hh?4"
      -- eval "hh?4??"
      -- eval "hh??0.87"

  describe "hybrid mod", ->
    it "should pass", ->
      -- eval "hh!!??"
      -- eval "hh!/2?!"

  describe "random seq", ->
    it "should pass", ->
      eval "bd | sd cp"

  describe "polyrhythm", ->
    it "should pass", ->
      eval "[bd sd] hh"
      eval "bd sd . cp . hh*2"
      eval "[bd, sd]"

  describe "polymeter", ->
    it "should pass", ->
      eval "bd*<2 3 4>"
      eval "{bd sd hh cp hh}%4"

export interpreter_targets = {
  -- numbers
  "45": pure 45
  "-2.": pure -2.0
  "4.64": pure 4.64
  "-3": pure -3
  -- word
  "foo": pure "foo"
  "Bar:2": C.s"Bar" .. C.n"2"
  -- rest
  "~": silence!
  -- modifiers
  "bd*2": fast 2, "bd"
  "bd/3": slow 3, "bd"
  "hh?": degrade "hh"
  "hh???": degrade degrade degrade "hh"
  -- "hh!!??": degrade degrade fastcat "hh", "hh", "hh" -- TODO: not right
  -- sequences
  "bd sd": fastcat "bd", "sd"
  "bd hh sd": fastcat "bd", "hh", "sd"
  "hh@2": pure "hh"
  "bd hh@2": timecat { { 1, "bd" }, { 2, "hh" } }
  -- TODO: timecat not right? mini is right
  -- "bd hh@3 sd@2": timecat { { 1, "bd" }, { 3, "hh" }, { 2, "sd" } }
  "hh!": fastcat "hh", "hh"
  "hh!!!": fastcat "hh", "hh", "hh", "hh"
  "bd! cp": fastcat "bd", "bd", "cp"
  "bd! hh? ~ sd/2 cp*3": timecat {
    { 1, "bd" }
    { 1, "bd" }
    { 1, degrade("hh") }
    { 1, silence! }
    { 1, slow(2, "sd") }
    { 1, fast(3, "cp") }
  }
  "bd | sd cp": randcat("bd", fastcat("sd", "cp"))
  "bd sd . cp . hh*2": fastcat(fastcat("bd", "sd"), "cp", fast(2, "hh"))
  "[bd, sd]": stack "bd", "sd"
  "[bd sd] hh": fastcat (fastcat "bd", "sd"), "hh"
  "{bd sd hh cp hh}%4": fastcat("bd", "sd", "hh", "cp")
  "bd*<2 3 4>": slowcat fast(2, "bd"), fast(3, "bd"), fast(4, "bd")
}

export visitor_targets = {
  -- numbers
  ["45"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "number", value: 45 },
        modifiers: {},
      },
    },
  },
  ["-2."]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "number", value: -2.0 },
        modifiers: {},
      },
    },
  },
  ["4.64"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "number", value: 4.64 },
        modifiers: {},
      },
    },
  },
  ["-3"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "number", value: -3 },
        modifiers: {},
      },
    },
  },
  -- words
  ["foo"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "foo", index: 0 },
        modifiers: {},
      },
    },
  },
  ["Bar:2"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "Bar", index: 2 },
        modifiers: {},
      },
    },
  },
  -- rest
  ["~"]: {
    type: "sequence",
    elements: {
      { type: "element", value: { type: "rest" }, modifiers: {} },
    },
  },
  -- modifiers
  ["bd*2"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "bd", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "fast",
            value: {
              type: "element",
              value: { type: "number", value: 2 },
              modifiers: {},
            },
          },
        },
      },
    },
  },
  ["bd/3"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "bd", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "slow",
            value: {
              type: "element",
              value: { type: "number", value: 3 },
              modifiers: {},
            },
          },
        },
      },
    },
  },
  ["hh?"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "degrade",
            value: {
              type: "degrade_arg",
              op: "count",
              value: 1,
            },
          },
        },
      },
    },
  },
  ["hh???"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "degrade",
            value: {
              type: "degrade_arg",
              op: "count",
              value: 3,
            },
          },
        },
      },
    },
  },
  ["hh?4"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "degrade",
            value: {
              type: "degrade_arg",
              op: "count",
              value: 4,
            },
          },
        },
      },
    },
  },
  ["hh?4??"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "degrade",
            value: {
              type: "degrade_arg",
              op: "count",
              value: 6,
            },
          },
        },
      },
    },
  },
  ["hh??0.87"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "degrade",
            value: {
              type: "degrade_arg",
              op: "value",
              value: 0.87,
            },
          },
        },
      },
    },
  },
  ["hh!"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: { { type: "modifier", op: "repeat", count: 1 } },
      },
    },
  },
  ["hh!!!"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: { { type: "modifier", op: "repeat", count: 3 } },
      },
    },
  },

  ["hh!4"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: { { type: "modifier", op: "repeat", count: 4 } },
      },
    },
  },

  ["hh!4!!"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: { { type: "modifier", op: "repeat", count: 6 } },
      },
    },
  },

  ["hh@2"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: { { type: "modifier", op: "weight", value: 2 } },
      },
    },
  },

  ["hh!!??!"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          { type: "modifier", op: "repeat", count: 2 },
          { type: "modifier", op: "repeat", count: 1 },
          {
            type: "modifier",
            op: "degrade",
            value: {
              type: "degrade_arg",
              op: "count",
              value: 2,
            },
          },
        },
      },
    },
  },

  -- sequences
  ["bd sd"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "bd", index: 0 },
        modifiers: {},
      },
      {
        type: "element",
        value: { type: "word", value: "sd", index: 0 },
        modifiers: {},
      },
    },
  },

  ["bd hh sd"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "bd", index: 0 },
        modifiers: {},
      },
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {},
      },
      {
        type: "element",
        value: { type: "word", value: "sd", index: 0 },
        modifiers: {},
      },
    },
  },

  ["bd! hh? ~ sd/2 cp*3"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "bd", index: 0 },
        modifiers: { { type: "modifier", op: "repeat", count: 1 } },
      },
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "degrade",
            value: {
              type: "degrade_arg",
              op: "count",
              value: 1,
            },
          },
        },
      },
      { type: "element", value: { type: "rest" }, modifiers: {} },
      {
        type: "element",
        value: { type: "word", value: "sd", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "slow",
            value: {
              type: "element",
              value: { type: "number", value: 2 },
              modifiers: {},
            },
          },
        },
      },
      {
        type: "element",
        value: { type: "word", value: "cp", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "fast",
            value: {
              type: "element",
              value: { type: "number", value: 3 },
              modifiers: {},
            },
          },
        },
      },
    },
  },
  ["[bd sd] hh"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: {
          type: "polyrhythm",
          seqs: {
            {
              type: "sequence",
              elements: {
                {
                  type: "element",
                  value: {
                    type: "word",
                    value: "bd",
                    index: 0,
                  },
                  modifiers: {},
                },
                {
                  type: "element",
                  value: {
                    type: "word",
                    value: "sd",
                    index: 0,
                  },
                  modifiers: {},
                },
              },
            },
          },
        },
        modifiers: {},
      },
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {},
      },
    },
  },
  -- random sequence
  ["bd | sd cp"]: {
    type: "random_sequence",
    elements: {
      {
        type: "sequence",
        elements: {
          {
            type: "element",
            value: { type: "word", value: "bd", index: 0 },
            modifiers: {},
          },
        },
      },
      {
        type: "sequence",
        elements: {
          {
            type: "element",
            value: { type: "word", value: "sd", index: 0 },
            modifiers: {},
          },
          {
            type: "element",
            value: { type: "word", value: "cp", index: 0 },
            modifiers: {},
          },
        },
      },
    },
  },
  ["bd _ _ sd"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "bd", index: 0 },
        modifiers: { { type: "modifier", op: "weight", value: 3 } },
      },
      {
        type: "element",
        value: { type: "word", value: "sd", index: 0 },
        modifiers: {},
      },
    },
  },
  ["bd sd . cp . hh*2"]: {
    elements: {
      {
        modifiers: {},
        type: "element",
        value: {
          seqs: {
            {
              elements: {
                {
                  modifiers: {},
                  type: "element",
                  value: {
                    index: 0,
                    type: "word",
                    value: "bd",
                  },
                },
                {
                  modifiers: {},
                  type: "element",
                  value: {
                    index: 0,
                    type: "word",
                    value: "sd",
                  },
                },
              },
              type: "sequence",
            },
          },
          type: "polyrhythm",
        },
      },
      {
        modifiers: {},
        type: "element",
        value: {
          seqs: {
            {
              elements: {
                {
                  modifiers: {},
                  type: "element",
                  value: {
                    index: 0,
                    type: "word",
                    value: "cp",
                  },
                },
              },
              type: "sequence",
            },
          },
          type: "polyrhythm",
        },
      },
      {
        modifiers: {},
        type: "element",
        value: {
          seqs: {
            {
              elements: {
                {
                  modifiers: {
                    {
                      op: "fast",
                      type: "modifier",
                      value: {
                        modifiers: {},
                        type: "element",
                        value: {
                          type: "number",
                          value: 2,
                        },
                      },
                    },
                  },
                  type: "element",
                  value: {
                    index: 0,
                    type: "word",
                    value: "hh",
                  },
                },
              },
              type: "sequence",
            },
          },
          type: "polyrhythm",
        },
      },
    },
    type: "sequence",
  },
  ["bd*<2 3 4>"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "bd", index: 0 },
        modifiers: {
          {
            type: "modifier",
            op: "fast",
            value: {
              type: "element",
              value: {
                type: "polymeter",
                seqs: {
                  {
                    type: "sequence",
                    elements: {
                      {
                        type: "element",
                        value: {
                          type: "number",
                          value: 2,
                        },
                        modifiers: {},
                      },
                      {
                        type: "element",
                        value: {
                          type: "number",
                          value: 3,
                        },
                        modifiers: {},
                      },
                      {
                        type: "element",
                        value: {
                          type: "number",
                          value: 4,
                        },
                        modifiers: {},
                      },
                    },
                  },
                },
                steps: 1,
              },
              modifiers: {},
            },
          },
        },
      },
    },
  },
  -- euclid_modifier
  ["1(3,8)"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "number", value: 1 },
        euclid_modifier: {
          type: "euclid_modifier",
          k: {
            type: "sequence",
            elements: { { type: "element", value: { type: "number", value: 3 }, modifiers: {} } },
          },
          n: {
            type: "sequence",
            elements: { { type: "element", value: { type: "number", value: 8 }, modifiers: {} } },
          },
        },
        modifiers: {},
      },
    },
  },
  ["{bd sd hh cp hh}%4"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: {
          type: "polymeter",
          seqs: {
            {
              type: "sequence",
              elements: {
                {
                  type: "element",
                  value: { type: "word", value: "bd", index: 0 },
                  modifiers: {},
                },
                {
                  type: "element",
                  value: { type: "word", value: "sd", index: 0 },
                  modifiers: {},
                },
                {
                  type: "element",
                  value: { type: "word", value: "hh", index: 0 },
                  modifiers: {},
                },
                {
                  type: "element",
                  value: { type: "word", value: "cp", index: 0 },
                  modifiers: {},
                },
                {
                  type: "element",
                  value: { type: "word", value: "hh", index: 0 },
                  modifiers: {},
                },
              },
            },
          },
          steps: 4,
        },
        modifiers: {},
      },
    },
  },
  ["[bd, sd]"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: {
          type: "polyrhythm",
          seqs: {
            {
              type: "sequence",
              elements: {
                {
                  type: "element",
                  value: { type: "word", value: "bd", index: 0 },
                  modifiers: {},
                },
              },
            },
            {
              type: "sequence",
              elements: {
                {
                  type: "element",
                  value: { type: "word", value: "sd", index: 0 },
                  modifiers: {},
                },
              },
            },
          },
        },
        modifiers: {},
      },
    },
  },
}
