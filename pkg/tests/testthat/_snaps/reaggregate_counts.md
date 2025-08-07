# reaggregate_count works for simple example without weights

    WAoAAAACAAQEAwACAwAAAAMTAAAABAAAAw0AAAAEAAAAAQAAAAIAAAADAAAABAAABAIAAAAB
    AAQACQAAAAZsZXZlbHMAAAAQAAAABAAEAAkAAAAGWzAsIDEpAAQACQAAAAZbMSwgMikABAAJ
    AAAABlsyLCA2KQAEAAkAAAAIWzYsIEluZikAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAA
    AgAEAAkAAAAHb3JkZXJlZAAEAAkAAAAGZmFjdG9yAAAA/gAAAA4AAAAEAAAAAAAAAAA/8AAA
    AAAAAEAAAAAAAAAAQBgAAAAAAAAAAAAOAAAABD/wAAAAAAAAQAAAAAAAAABAGAAAAAAAAH/w
    AAAAAAAAAAAADgAAAAQAAAAAAAAAAD/4AAAAAAAAQCcAAAAAAABAJAAAAAAAAAAABAIAAAL/
    AAAAEAAAAAMABAAJAAAABnRibF9kZgAEAAkAAAADdGJsAAQACQAAAApkYXRhLmZyYW1lAAAE
    AgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAAAP////wAAAQCAAAAAQAEAAkAAAAF
    bmFtZXMAAAAQAAAABAAEAAkAAAAIaW50ZXJ2YWwABAAJAAAABWxvd2VyAAQACQAAAAV1cHBl
    cgAEAAkAAAAFY291bnQAAAD+

# reaggregate_count works with weights and with the population_bounds equal to bounds

    WAoAAAACAAQEAwACAwAAAAMTAAAABAAAAw0AAAAEAAAAAQAAAAIAAAADAAAABAAABAIAAAAB
    AAQACQAAAAZsZXZlbHMAAAAQAAAABAAEAAkAAAAGWzAsIDEpAAQACQAAAAZbMSwgMikABAAJ
    AAAABlsyLCA2KQAEAAkAAAAIWzYsIEluZikAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAA
    AgAEAAkAAAAHb3JkZXJlZAAEAAkAAAAGZmFjdG9yAAAA/gAAAA4AAAAEAAAAAAAAAAA/8AAA
    AAAAAEAAAAAAAAAAQBgAAAAAAAAAAAAOAAAABD/wAAAAAAAAQAAAAAAAAABAGAAAAAAAAH/w
    AAAAAAAAAAAADgAAAAQAAAAAAAAAAEACAAAAAAAAQCWAAAAAAABAJAAAAAAAAAAABAIAAAL/
    AAAAEAAAAAMABAAJAAAABnRibF9kZgAEAAkAAAADdGJsAAQACQAAAApkYXRhLmZyYW1lAAAE
    AgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAAAP////wAAAQCAAAAAQAEAAkAAAAF
    bmFtZXMAAAAQAAAABAAEAAkAAAAIaW50ZXJ2YWwABAAJAAAABWxvd2VyAAQACQAAAAV1cHBl
    cgAEAAkAAAAFY291bnQAAAD+

---

    WAoAAAACAAQEAwACAwAAAAMTAAAABAAAAw0AAAAEAAAAAQAAAAIAAAADAAAABAAABAIAAAAB
    AAQACQAAAAZsZXZlbHMAAAAQAAAABAAEAAkAAAAGWzAsIDEpAAQACQAAAAZbMSwgMikABAAJ
    AAAABlsyLCA2KQAEAAkAAAAIWzYsIEluZikAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAA
    AgAEAAkAAAAHb3JkZXJlZAAEAAkAAAAGZmFjdG9yAAAA/gAAAA4AAAAEAAAAAAAAAAA/8AAA
    AAAAAEAAAAAAAAAAQBgAAAAAAAAAAAAOAAAABD/wAAAAAAAAQAAAAAAAAABAGAAAAAAAAH/w
    AAAAAAAAAAAADgAAAAQAAAAAAAAAAEACAAAAAAAAQCWAAAAAAABAJAAAAAAAAAAABAIAAAL/
    AAAAEAAAAAMABAAJAAAABnRibF9kZgAEAAkAAAADdGJsAAQACQAAAApkYXRhLmZyYW1lAAAE
    AgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAAAP////wAAAQCAAAAAQAEAAkAAAAF
    bmFtZXMAAAAQAAAABAAEAAkAAAAIaW50ZXJ2YWwABAAJAAAABWxvd2VyAAQACQAAAAV1cHBl
    cgAEAAkAAAAFY291bnQAAAD+

# reaggregate_counts errors as expected

    Code
      reaggregate_counts(bounds = c(0, 80, 150, Inf), counts = counts, new_bounds = new_bounds,
      population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `bounds` must be a finite, numeric vector.

---

    Code
      reaggregate_counts(bounds = integer(), counts = counts, new_bounds = new_bounds,
      population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `bounds` must be of non-zero length.

---

    Code
      reaggregate_counts(bounds = rev(bounds), counts = counts, new_bounds = new_bounds,
      population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `bounds` must be in strictly ascending order

---

    Code
      reaggregate_counts(bounds = c(-1, 80, 150, 180), counts = counts, new_bounds = new_bounds,
      population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `bounds` must be non-negative.

---

    Code
      reaggregate_counts(bounds = bounds, counts = letters, new_bounds = new_bounds,
        population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `counts` must be numeric.

---

    Code
      reaggregate_counts(bounds = bounds[-1L], counts = counts, new_bounds = new_bounds,
      population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `counts` must be the same length as `bounds`.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = c(0, 60, 150,
        160, Inf), population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `new_bounds` must be a finite, numeric vector.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = integer(),
      population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `new_bounds` must be of non-zero length.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = rev(
        new_bounds), population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `new_bounds` must be in strictly ascending order

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = c(-1, 60, 150,
        160, 180), population_bounds = population_bounds, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `new_bounds` must be non-negative.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = population_bounds, population_weights = population_weights[
          -1L])
    Condition
      Error in `reaggregate_counts()`:
      ! `population_weights` must be the same length as `population_bounds`.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = NULL, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! When `population_bounds` is not specified, `population_weights` must be the same length as `new_bounds`.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = c(0, 60, 150,
        160, 181), population_bounds = NULL, population_weights = population_weights[
        -1L])
    Condition
      Error in `reaggregate_counts()`:
      ! Where `population_bounds` are not specified the maximum value of `new_bounds` must be less than or equal to that of `bounds`.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = Inf, population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `population_bounds` must be a finite, numeric vector.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = numeric(), population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `population_bounds` must be of non-zero length.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = rev(population_bounds), population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `population_bounds` must be in strictly ascending order

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = c(-1, 60, 150, 160, 175, 180), population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! `population_bounds} must be non-negative.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = c(0, 60, 150, 160, 175, 179), population_weights = population_weights)
    Condition
      Error in `reaggregate_counts()`:
      ! The maximum value of `bounds` must be less than or equal to that of `population_bounds`.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = population_bounds, population_weights = c(10, 20, 30, 40,
          50, Inf))
    Condition
      Error in `reaggregate_counts()`:
      ! `population_weights` must be numeric, non-negative and finite.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = population_bounds, population_weights = c(-10, 20, 30, 40,
          50, 60))
    Condition
      Error in `reaggregate_counts()`:
      ! `population_weights` must be numeric, non-negative and finite.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = population_bounds, population_weights = population_weights[
          -1L])
    Condition
      Error in `reaggregate_counts()`:
      ! `population_weights` must be the same length as `population_bounds`.

---

    Code
      reaggregate_counts(bounds = bounds, counts = counts, new_bounds = new_bounds,
        population_bounds = population_bounds, population_weights = numeric(length(
          population_bounds)))
    Condition
      Error in `reaggregate_counts()`:
      ! At least one `population_weight` must be non-zero.

