# without weights
result_1 <- split_interval_counts(
    lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
    upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
    counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
)

expected_1 <- data.frame(
    age = c(
        1, 2,
        2,
        NA_integer_,
        NA_integer_,
        1, 2,
        NA_integer_,
        4, 5
    ),
    count = c(
        0.5, 0.5,
        1,
        1,
        1,
        0.5, 0.5,
        1,
        NA_real_, NA_real_
    )
)

expect_equal(result_1, expected_1)

# with weights
result_2 <- split_interval_counts(
    lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
    upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
    counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
    weights = c(1, 3, 1, 1, 1, 1),
    max_upper = 6
)

expected_2 <- data.frame(
    age = c(
        1, 2,
        2,
        NA_integer_,
        NA_integer_,
        1, 2,
        NA_integer_,
        4, 5
    ),
    count = c(
        0.75, 0.25,
        1,
        1,
        1,
        0.75, 0.25,
        1,
        NA_real_, NA_real_
    )
)

expect_equal(result_2, expected_2)

# zero weights
result_3 <- split_interval_counts(
    lower_bounds = c(1L, 6L),
    upper_bounds = c(6L, 11L),
    counts = c(5L, 5L),
    max_upper = 11,
    weights = c(rep.int(1L, 6L), rep.int(0L, 5L))
)
expected_3 <- data.frame(
    age = 1:10,
    count = c(rep.int(1L, 5L), rep.int(0L, 5L))
)
expect_equal(result_3, expected_3)

# input checking
expect_error(
    split_interval_counts(
        lower_bounds = as.character(c(1, 2, NA,  2, 1, NA, 4)),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    ),
    "`lower_bounds` must be numeric.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = as.character(c(3, 3,  1, NA, 3, NA, 6)),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    ),
    "`upper_bounds` must be numeric.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       as.character(c(1, 1,  1,  1, 1,  1, NA_real_))
    ),
    "`counts` must be numeric.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = "100"
    ),
    "`max_upper` must be an integer of length 1.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = 100:101
    ),
    "`max_upper` must be an integer of length 1.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = 201
    ),
    "`max_upper` must be less than or equal to 200.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 7),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    ),
    "`lower_bounds` must be less than `upper_bounds`.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    ),
    "`lower_bounds` and `upper_bounds` must be the same length.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 101),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    ),
    "`upper_bounds` can not be greater than `max_upper` unless infinite.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1)
    ),
    "`bounds` and `counts` must be the same length.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = 50,
        weights = 201
    ),
    "`weights` must be a vector of length 50 (`max_upper`) representing ages 0:49",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        weights = ""
    ),
    "`weights` must be numeric.",
    fixed = TRUE
)
max_upper <- 50L
weights <- seq_len(max_upper)
weights[1] <- - weights[1]
expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = max_upper,
        weights = weights
    ),
    "`weights` must be positive and not missing (NA).",
    fixed = TRUE
)

weights[1] <- NA_integer_
expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = max_upper,
        weights = weights
    ),
    "`weights` must be positive and not missing (NA).",
    fixed = TRUE
)

# check success
expect_silent(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, Inf),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    )
)
