# split_interval_counts works

    Code
      dat <- split_interval_counts(lower_bounds = c(1, 2, 1, 4), upper_bounds = c(3,
        3, 3, Inf), counts = c(1, 1, 1, NA_real_))
    Condition
      Warning in `split_interval_counts()`:
      split_interval_counts() was deprecated in the 0.0.5 release of ageutils and may be removed in a future release.
      Warning in `split_interval_counts()`:
      `upper_bounds` greater than `max_upper` (100) have been replaced prior to splitting.

