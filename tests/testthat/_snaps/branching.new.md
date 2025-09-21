# gen_expbranches() works

    Code
      gen_expbranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
             x1      x2       x3       x4
          <dbl>   <dbl>    <dbl>    <dbl>
       1 -1.61  18.1    -0.00981 -0.135  
       2  1.23   0.172   0.0122   0.0304 
       3  1.60   0.0831 -0.140   -0.0537 
       4 -0.138  1.28    0.0553  -0.0138 
       5  0.588  0.434  -0.0447  -0.0437 
       6  0.236  0.710  -0.0402  -0.0389 
       7 -0.245  1.59    0.00297 -0.00582
       8 -0.368  2.00   -0.0240   0.0174 
       9 -1.75  23.4     0.0514   0.00884
      10 -1.31  10.6    -0.0438   0.0173 
      # i 390 more rows

# gen_orgcurvybranches() works

    Code
      gen_orgcurvybranches(n = 400, p = 4, k = 4)
    Condition
      Warning:
      The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
      i Using compatibility `.name_repair`.
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
               x1       x2    x3      x4
            <dbl>    <dbl> <dbl>   <dbl>
       1 -0.140   -0.0537  1.29  -2.08  
       2  0.0553  -0.0138  1.12  -1.59  
       3 -0.0447  -0.0437  0.877 -0.966 
       4 -0.0402  -0.0389  0.816 -0.673 
       5  0.00297 -0.00582 0.125  0.261 
       6 -0.0240   0.0174  0.344  0.0121
       7  0.0514   0.00884 1.30  -2.51  
       8 -0.0438   0.0173  1.20  -1.80  
       9  0.0609  -0.00164 0.642 -0.540 
      10 -0.00698  0.0421  0.937 -1.14  
      # i 390 more rows

# gen_orglinearbranches() works

    Code
      gen_orglinearbranches(n = 400, p = 4, k = 4)
    Condition
      Warning:
      The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
      i Using compatibility `.name_repair`.
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
               x1       x2    x3      x4
            <dbl>    <dbl> <dbl>   <dbl>
       1 -0.140   -0.0537  1.29  -1.50  
       2  0.0553  -0.0138  1.12  -1.40  
       3 -0.0447  -0.0437  0.877 -1.13  
       4 -0.0402  -0.0389  0.816 -0.898 
       5  0.00297 -0.00582 0.125  0.0966
       6 -0.0240   0.0174  0.344 -0.327 
       7  0.0514   0.00884 1.30  -1.92  
       8 -0.0438   0.0173  1.20  -1.45  
       9  0.0609  -0.00164 0.642 -0.884 
      10 -0.00698  0.0421  0.937 -1.23  
      # i 390 more rows

# gen_linearbranches() works

    Code
      gen_linearbranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
             x1      x2       x3       x4
          <dbl>   <dbl>    <dbl>    <dbl>
       1  7.06   4.03    0.0927   0.0187 
       2  3.70   2.15   -0.0614  -0.0355 
       3  0.604  0.600  -0.00981 -0.135  
       4  1.57   1.06    0.0122   0.0304 
       5 -1.02  -0.0783 -0.140   -0.0537 
       6  6.08   3.36    0.0553  -0.0138 
       7  7.01   3.64   -0.0447  -0.0437 
       8  2.66   1.33   -0.0402  -0.0389 
       9  4.47   2.67    0.00297 -0.00582
      10  3.59   2.07   -0.0240   0.0174 
      # i 390 more rows

# gen_curvybranches() works

    Code
      gen_curvybranches(n = 400, p = 4, k = 4)
    Message
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v 2 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 400 x 4
             x1     x2       x3       x4
          <dbl>  <dbl>    <dbl>    <dbl>
       1 0.906  0.962   0.0927   0.0187 
       2 0.570  0.411  -0.0614  -0.0355 
       3 0.260  0.124  -0.00981 -0.135  
       4 0.357  0.191   0.0122   0.0304 
       5 0.0982 0.0625 -0.140   -0.0537 
       6 0.808  0.766   0.0553  -0.0138 
       7 0.901  0.915  -0.0447  -0.0437 
       8 0.466  0.264  -0.0402  -0.0389 
       9 0.647  0.527   0.00297 -0.00582
      10 0.559  0.396  -0.0240   0.0174 
      # i 390 more rows

