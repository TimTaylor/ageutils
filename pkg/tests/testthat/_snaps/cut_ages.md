# Errors work as expected

    Code
      cut_ages("bob")
    Condition
      Error in `cut_ages()`:
      ! argument "breaks" is missing, with no default

---

    Code
      cut_ages("bob", 3)
    Condition
      Error in `cut_ages()`:
      ! `ages` must be numeric and of length >= 1.

---

    Code
      cut_ages(3, 3, TRUE)
    Condition
      Error in `cut_ages()`:
      ! `max_upper` must be positive, numeric and of length 1.

---

    Code
      cut_ages(3, 3, NA_real_)
    Condition
      Error in `cut_ages()`:
      ! `max_upper` must be positive, numeric and of length 1.

---

    Code
      cut_ages(3, 3, 1:2)
    Condition
      Error in `cut_ages()`:
      ! `max_upper` must be positive, numeric and of length 1.

---

    Code
      cut_ages(1:10, breaks = "5L")
    Condition
      Error in `cut_ages()`:
      ! `breaks` must be numeric and of length >= 1.

---

    Code
      cut_ages(-1:10, 5L)
    Condition
      Error in `cut_ages()`:
      ! `ages` must greater than or equal to the minimum value of `breaks`.

---

    Code
      cut_ages(1:10, breaks = NA_integer_)
    Condition
      Error in `cut_ages()`:
      ! `breaks` must be coercible to integer, non-negative and not NA.

---

    Code
      cut_ages(1:10, breaks = c(2L, 2L))
    Condition
      Error in `cut_ages()`:
      ! `ages` must greater than or equal to the minimum value of `breaks`.

---

    Code
      cut_ages(c(1:5, 99:102), c(3L, 98L))
    Condition
      Error in `cut_ages()`:
      ! `ages` must greater than or equal to the minimum value of `breaks`.

---

    Code
      cut_ages(c(NA_integer_, 2:5, 99:102), c(0L, 3L, 98L))
    Condition
      Error in `cut_ages()`:
      ! `ages` must be coercible to integer and not NA.

