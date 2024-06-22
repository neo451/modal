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
}
