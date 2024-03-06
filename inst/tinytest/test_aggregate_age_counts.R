# single break
dat <- 1:10
brk <- c(0, 5L)
expected <- data.frame(
    interval = factor(c("[0, 5)", "[5, Inf)"), ordered = TRUE),
    lower_bound = c(0, 5),
    upper_bound = c(5, Inf),
    count = c(15, 40)
)
expect_equal(aggregate_age_counts(dat, breaks = brk), expected)


# NA ages are handled
counts <- ages <- 1:65
ages[1:44] <- NA
expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 5)", "[5, 15)", "[15, 25)", "[25, 45)", "[45, 65)", "[65, Inf)", NA_character_),
        levels = c("[0, 1)", "[1, 5)", "[5, 15)", "[15, 25)", "[25, 45)", "[45, 65)", "[65, Inf)", NA_character_),
        ordered = TRUE
    ),
    lower_bound = c(0, 1, 5, 15, 25, 45, 65, NA),
    upper_bound = c(1, 5, 15, 25, 45, 65, Inf, NA),
    count = c(0, 0, 0, 0, 0, sum(45:64), 65, sum(1:44))
)

expect_equal(
    aggregate_age_counts(counts, ages, breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L)),
    expected
)

# no need for ages to be consecutive
counts <- ages <- c(1, 10)
breaks <- c(0, counts)
expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 10)", "[10, Inf)"),
        levels = c("[0, 1)", "[1, 10)", "[10, Inf)"),
        ordered = TRUE
    ),
    lower_bound = c(0, 1, 10),
    upper_bound = c(1, 10, Inf),
    count = c(0, 1, 10)
)
expect_equal(aggregate_age_counts(counts, ages, breaks), expected)

# counts and ages do not need to be ordered
counts <- ages <- c(10, 1)
breaks <- c(0, 1, 10)
expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 10)", "[10, Inf)"),
        levels = c("[0, 1)", "[1, 10)", "[10, Inf)"),
        ordered = TRUE
    ),
    lower_bound = c(0, 1, 10),
    upper_bound = c(1, 10, Inf),
    count = c(0, 1, 10)
)
expect_equal(aggregate_age_counts(counts, ages, breaks), expected)

counts <- ages <- c(10, 1)
breaks <- c(3, 10)
expect_error(
    aggregate_age_counts(counts, ages, breaks),
    "`ages` must greater than or equal to the minimum value of `breaks`.",
    fixed = TRUE
)


# error messaging
counts <- ages <- c(10, 1)
breaks <- c(0, counts)
expect_error(
    aggregate_age_counts(counts, ages, breaks),
    "`breaks` must be in strictly increasing order.",
    fixed = TRUE
)

expect_error( aggregate_age_counts(1:10, as.character(1:10), 5L) )

expect_error(
    aggregate_age_counts(1:10, 6:14, 5L),
    "`ages` and `counts` must be the same length.",
    fixed = TRUE
)

expect_error( aggregate_age_counts("bob", breaks = 1L) )

expect_error(
    aggregate_age_counts(1:10, breaks = NA_integer_),
    "`breaks` must be non-missing (not NA) and coercible to integer.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(1:10, breaks = c(2L, 2L)),
    "`breaks` must be in strictly increasing order.",
    fixed = TRUE
)

expect_error( aggregate_age_counts(1:10, breaks = "5") )


# success
expect_silent(aggregate_age_counts(1:10, rep.int(NA_integer_, 10L), breaks = 2L))

