# without weights
lower_bounds <- c(1, 2, NA,  2, 1, NA, 4, 6)
upper_bounds <- c(3, 3,  1, NA, 3, NA, 6, Inf)
counts <- c(1, 1,  1,  1, 1,  1, NA_real_, 10)
max_upper <- 100
breaks <- c(0, 1, 2, 6)

expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)", NA_character_),
        levels = c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)"),
        ordered = TRUE
    ),
    lower_bound = c(0, 1, 2, 6, NA),
    upper_bound = c(1, 2, 6, Inf, NA),
    count = c(0, 1, NA, 10, 3)
)

expect_equal(
    reaggregate_interval_counts(
        lower_bounds = lower_bounds,
        upper_bounds = upper_bounds,
        counts = counts,
        breaks = breaks,
        max_upper = max_upper
    ),
    expected
)

# with weights
weights <- c(1, 3, rep.int(1, 98))

expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)", NA_character_),
        levels = c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)"),
        ordered = TRUE
    ),
    lower_bound = c(0, 1, 2, 6, NA),
    upper_bound = c(1, 2, 6, Inf, NA),
    count = c(0, 1.5, NA, 10, 3)
)

expect_equal(
    reaggregate_interval_counts(
        lower_bounds = lower_bounds,
        upper_bounds = upper_bounds,
        counts = counts,
        breaks = breaks,
        weights = weights,
        max_upper = max_upper
    ),
    expected
)
