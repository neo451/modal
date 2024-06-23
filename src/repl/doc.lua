return {
   slowcat = {
      "cat pattern per cycle",
      "pats :: [Pattern]",
      [[ slowcat {1, 2, 3} ]],
      [[ slowcat '(1 2 3) ]],
   },
   fast = {
      "make pattern play n times faster",
      "factor :: number, pat :: any",
      [[ fast(2, 1) | pure(1):fast(2) ]],
      [[ fast 2 1 | (fast 2 1) | ((fast 2) 1)]],
   },

   fastgap = {
      "speeds up a pattern like fast, but rather than it playing multiple times as fast would it instead leaves a gap in the remaining space of the cycle. For example, the following will play the sound pattern 'bd sn' only once but compressed into the first half of the cycle, i.e. twice as fast.",
      { factor = "number", pat = "any" },
   },
}
