import compare, type from require("xi.utils")
import Fraction from require "xi.fraction"

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
