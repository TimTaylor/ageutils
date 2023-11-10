# without weights
result_1 <- split_interval_counts(
    lower_bounds = c(1, 2, 1, 4),
    upper_bounds = c(3, 3, 3, 6),
    counts =       c(1, 1, 1, NA_real_)
)

expected_1 <- data.frame(
    age = c(
        1, 2,
        2,
        1, 2,
        4, 5
    ),
    count = c(
        0.5, 0.5,
        1,
        0.5, 0.5,
        NA_real_, NA_real_
    )
)

expect_equal(result_1, expected_1)
expect_identical(
    result_1,
    ageutils:::split_interval_counts_r(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_)
    )
)

# with weights
result_2 <- split_interval_counts(
    lower_bounds = c(1, 2, 1, 4),
    upper_bounds = c(3, 3, 3, 6),
    counts =       c(1, 1, 1, NA_real_),
    weights = c(1, 3, 1, 1, 1, 1),
    max_upper = 6
)

expected_2 <- data.frame(
    age = c(
        1, 2,
        2,
        1, 2,
        4, 5
    ),
    count = c(
        0.75, 0.25,
        1,
        0.75, 0.25,
        NA_real_, NA_real_
    )
)

expect_equal(result_2, expected_2)
expect_identical(
    result_2,
    ageutils:::split_interval_counts_r(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_),
        weights = c(1, 3, 1, 1, 1, 1),
        max_upper = 6
    )
)

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
expect_identical(
    result_3,
    ageutils:::split_interval_counts_r(
        lower_bounds = c(1L, 6L),
        upper_bounds = c(6L, 11L),
        counts = c(5L, 5L),
        max_upper = 11,
        weights = c(rep.int(1L, 6L), rep.int(0L, 5L))
    )
)


# input checking
expect_error(
    split_interval_counts(
        lower_bounds = as.character(c(1, 2, NA,  2, 1, NA, 4)),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    ),
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = as.character(c(3, 3,  1, NA, 3, NA, 6)),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
    )
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       as.character(c(1, 1,  1,  1, 1,  1, NA_real_))
    )
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = "100"
    )
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
        max_upper = 100:101
    )
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, 1, 7),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_)
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
        upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
        counts =       c(1, 1,  1,  1, 1,  1)
    ),
    "`bounds` and `counts` must be the same length.",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_),
        max_upper = 50,
        weights = 201
    ),
    "`weights` must be a vector of length 50 (`max_upper`) representing ages 0:49",
    fixed = TRUE
)

expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_),
        weights = ""
    )
)

max_upper <- 50L
weights <- seq_len(max_upper)
weights[1] <- - weights[1]
expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_),
        max_upper = max_upper,
        weights = weights
    )
)

weights[1] <- NA_integer_
expect_error(
    split_interval_counts(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_),
        max_upper = max_upper,
        weights = weights
    )
)

# check success
expect_warning(
    split_interval_counts(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, Inf),
        counts =       c(1, 1, 1, NA_real_)
    ),
    "`upper_bounds` greater than `max_upper` (100) have been replaced prior to splitting.",
    fixed = TRUE
)
