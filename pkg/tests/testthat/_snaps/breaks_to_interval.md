# Errors work as expected

    Code
      breaks_to_interval(c(Inf, 0, 10.3, 1000))
    Condition
      Warning in `breaks_to_interval()`:
      NAs introduced by coercion to integer range
      Error in `breaks_to_interval()`:
      ! `breaks` must be finite, and, coercible to integer.

---

    Code
      breaks_to_interval(c(NA_real_, 0, 10.3, 1000))
    Condition
      Error in `breaks_to_interval()`:
      ! `breaks` must be finite, and, coercible to integer.

---

    Code
      breaks_to_interval(c(1, 0, 10.3, 1000))
    Condition
      Error in `breaks_to_interval()`:
      ! `breaks` must be in strictly increasing order.

---

    Code
      breaks_to_interval(c(0, 0, 10.3, 1000))
    Condition
      Error in `breaks_to_interval()`:
      ! `breaks` must be in strictly increasing order.

---

    Code
      breaks_to_interval(TRUE)
    Condition
      Error in `breaks_to_interval()`:
      ! `breaks` must be numeric.

---

    Code
      breaks_to_interval(.Machine$integer.max + 1)
    Condition
      Warning in `breaks_to_interval()`:
      NAs introduced by coercion to integer range
      Error in `breaks_to_interval()`:
      ! `breaks` must be finite, and, coercible to integer.

---

    Code
      breaks_to_interval(1, TRUE)
    Condition
      Error in `breaks_to_interval()`:
      ! `max_upper` must be a numeric vector of length 1 and not NA.

---

    Code
      breaks_to_interval(1, 1:2)
    Condition
      Error in `breaks_to_interval()`:
      ! `max_upper` must be a numeric vector of length 1 and not NA.

---

    Code
      breaks_to_interval(1, NA_real_)
    Condition
      Error in `breaks_to_interval()`:
      ! `max_upper` must be a numeric vector of length 1 and not NA.

---

    Code
      breaks_to_interval(1, 1)
    Condition
      Error in `breaks_to_interval()`:
      ! `max_upper` must be greater than all `breaks`.

