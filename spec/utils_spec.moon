import compare, type from require("xi.utils")

require "xi.fraction"

describe "compare", ->
  it "should deeply compare table values", ->
    table1 = { first: "red fish", second: { low: 5 } }
    table2 = { first: "red fish", second: { low: 5 } }
    table3 = { first: "blue fish", second: { low: 5 } }
    table4 = { first: "red fish", second: { low: 6 } }
    table5 = {}
    table6 = {}
    assert.is_true compare table1, table2
    assert.is_true compare table5, table6
    assert.is_false compare table1, table3
    assert.is_False compare table1, table4
    assert.is_False compare table1, table5

describe "Type", ->
  it "should return the tranquility class type or the value of type operator if not defined", ->
    hasClassType = Fraction()
    doesNotHaveClassType = { thing: "value" }
    doesNotHaveClassType2 = 3
    assert.are.equal "fraction", type(hasClassType)
    assert.are.equal "table", type(doesNotHaveClassType)
    assert.are.equal "number", type(doesNotHaveClassType2)
