require("xi.mini.visitor")

same = (name) -> assert.same(visitor_targets[name], Parse_mini(name))

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
      same "hh!/2?!"

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


eval = (input, query_span, expected_pat) ->
	pat = Mini input
	span = query_span or TimeSpan:new 0, 1
	assert.same(expected_pat, pat.query span)

interpreter_targets = {
	-- { "45", nil, Pure 45 },
	-- 	{ "-2.", nil, Pure(-2.0) },
	-- 	{ "4.64", nil, Pure(4.64) },
	-- 	{ "-3", nil, Pure(-3) },
	-- 	-- words
	-- 	{ "foo", nil, Pure("foo") },
	-- 	{ "Bar", nil, Pure("Bar") },
	-- 	-- rest
	-- 	{ "~", nil, Silence() },
	-- 	-- modifiers
	-- 	{ "bd*2", nil, Fast(2, "bd") },
	-- 	{ "bd/3", nil, Slow(3, "bd") },
	-- 	{ "hh?", nil, Degrade("hh") },
	-- 	{ "hh??", nil, Degrade(Degrade("hh")) },
	-- 	{
	-- 		"hh!!??",
	-- 		nil,
	-- 		Degrade({ Degrade(Fastcat({ "hh", "hh", "hh" })) }),
	-- 	},
	-- 	-- sequences
	-- 	{ "bd sd", nil, Fastcat({ "bd", "sd" }) },
	-- 	{ "bd hh sd", nil, Fastcat({ "bd", "hh", "sd" }) },
	-- 	{ "hh@2", nil, Pure("hh") },
	-- 	{ "bd hh@2", nil, Timecat({ { 1, "bd" }, { 2, "hh" } }) },
	-- 	{ "bd hh@3 sd@2", nil, Timecat({ { 1, "bd" }, { 3, "hh" }, { 2, "sd" } }) },
	-- 	{ "hh!", nil, Fastcat({ "hh", "hh" }) },
	-- 	{ "hh!!", nil, Fastcat({ "hh", "hh", "hh" }) },
	-- 	{ "bd! cp", nil, Fastcat({ "bd", "bd", "cp" }) },
	-- 	{
	-- 		"bd! hh? ~ sd/2 cp*3",
	-- 		nil,
	-- 		Timecat({
	-- 			{ 1, "bd" },
	-- 			{ 1, "bd" },
	-- 			{ 1, Degrade("hh") },
	-- 			{ 1, Silence() },
	-- 			{ 1, Slow(2, "sd") },
	-- 			{ 1, Fast(3, "cp") },
	-- 			{ 1, Fast(3, "cp") },
	-- 			{ 1, Fast(3, "cp") },
	-- 		}),
	-- 	},
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

  ["hh!/2?!"]: {
    type: "sequence",
    elements: {
      {
        type: "element",
        value: { type: "word", value: "hh", index: 0 },
        modifiers: {
          { type: "modifier", op: "repeat", count: 1 },
          {
            type: "modifier",
            op: "slow",
            value: {
              type: "element",
              value: { type: "number", value: 2 },
              modifiers: {
                {
                  type: "modifier",
                  op: "repeat",
                  count: 1,
                },
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
