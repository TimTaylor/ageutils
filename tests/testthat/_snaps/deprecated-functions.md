# aggregate_age_counts works

    Code
      suppressWarnings(aggregate_age_counts(counts = c(10, 1), ages = c(10, 1),
      breaks = c(3, 10)))
    Condition
      Error in `aggregate_age_counts()`:
      ! `ages` must greater than or equal to the minimum value of `breaks`.

---

    Code
      suppressWarnings(aggregate_age_counts(counts = c(10, 1), ages = c(10, 1),
      breaks = c(0, 10, 1)))
    Condition
      Error in `aggregate_age_counts()`:
      ! `breaks` must be in strictly increasing order.

---

    Code
      suppressWarnings(aggregate_age_counts(counts = 1:10, ages = as.character(1:10),
      breaks = 5L))
    Condition
      Error in `aggregate_age_counts()`:
      ! `ages` must be numeric and of length >= 1.

---

    Code
      suppressWarnings(aggregate_age_counts(1:10, 6:14, 5L))
    Condition
      Error in `aggregate_age_counts()`:
      ! `ages` and `counts` must be the same length.

---

    Code
      suppressWarnings(aggregate_age_counts("bob", breaks = 1L))
    Condition
      Error in `aggregate_age_counts()`:
      ! `counts` must be numeric and of length >= 1.

---

    Code
      suppressWarnings(aggregate_age_counts(1:10, breaks = NA_integer_))
    Condition
      Error in `aggregate_age_counts()`:
      ! `breaks` must be non-missing (not NA) and coercible to integer.

---

    Code
      suppressWarnings(aggregate_age_counts(1:10, breaks = c(2L, 2L)))
    Condition
      Error in `aggregate_age_counts()`:
      ! `breaks` must be in strictly increasing order.

---

    Code
      suppressWarnings(aggregate_age_counts(1:10, breaks = "5"))
    Condition
      Error in `aggregate_age_counts()`:
      ! `breaks` must be numeric and of length >= 1.

# split_interval_counts works

    Code
      dat <- split_interval_counts(lower_bounds = c(1, 2, 1, 4), upper_bounds = c(3,
        3, 3, Inf), counts = c(1, 1, 1, NA_real_))
    Condition
      Warning in `split_interval_counts()`:
      split_interval_counts() was deprecated in the 0.0.5 release of ageutils and may be removed in a future release.
      Warning in `split_interval_counts()`:
      `upper_bounds` greater than `max_upper` (100) have been replaced prior to splitting.

