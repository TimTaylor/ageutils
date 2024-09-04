# without weights
lower_bounds <- c(1, 3,  4,   6)
upper_bounds <- c(3, 4,  6, Inf)
counts       <- c(3, 0, 10,  10)
breaks       <- c(0, 1,  2,   6)

target <- suppressWarnings(
    tibble::as_tibble(
        reaggregate_interval_counts(
            lower_bounds = lower_bounds,
            upper_bounds = upper_bounds,
            counts = counts,
            breaks = breaks
        )
    )
)

current <- reaggregate_counts(
    bounds = lower_bounds,
    counts = counts,
    new_bounds = breaks
)

expect_equal(
    current,
    setNames(target, names(current))
)


# with weights
weights <- c(1, 3, rep.int(1, 98))

target <- suppressWarnings(
    tibble::as_tibble(
        reaggregate_interval_counts(
            lower_bounds = lower_bounds,
            upper_bounds = upper_bounds,
            counts = counts,
            breaks = breaks,
            weights = weights
        )
    )
)

current <- reaggregate_counts(
    bounds = lower_bounds,
    counts = counts,
    new_bounds = breaks,
    population_bounds = breaks,
    population_weights = c(1, 3, 4, 94)
)

expect_equal(
    current,
    setNames(target, names(current))
)

