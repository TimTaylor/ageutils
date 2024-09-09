test_that("reaggregate_count works for simple example without weights", {

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
})

test_that("reaggregate_count matches Edwins for simple example without weights", {

    skip_if_not_installed("dplyr")

    lower_bounds <- c(1, 3,  4,   6)
    upper_bounds <- c(3, 4,  6, Inf)
    counts       <- c(3, 0, 10,  10)
    breaks       <- c(0, 1,  2,   6)

    current <- reaggregate_counts(
        bounds = lower_bounds,
        counts = counts,
        new_bounds = breaks
    )

    target <- reaggregate_counts_edwin_unweighted(
        bounds = c(0, lower_bounds),
        counts = c(0, counts),
        new_bounds = breaks
    )

    current <- current[-c(1,3)]
    expect_equal(
        current,
        setNames(target, names(current))
    )
})


test_that("reaggregate_count works where the original bounds contain the new with no overlaps", {

    original_bounds <- c(0, 80, 150, 180)
    original_counts <- c(10, 20, 30, 40)

    dat <- reaggregate_counts(
        bounds = original_bounds,
        counts = original_counts,
        new_bounds = c(0, 60, 80, 150, 160, 180),
        population_bounds = c(0, 60, 150, 160, 175, 180),
        population_weights = c(10, 20, 30, 40, 50, 60)
    )

    out <- with(
        dat,
        reaggregate_counts(
            bounds = lower,
            counts = count,
            new_bounds = original_bounds,
            population_bounds = original_bounds,
            population_weights = original_counts
        )
    )
    expect_equal(out$lower, original_bounds)
    expect_equal(out$count, original_counts)

})

test_that("reaggregate_count works with weights and with the population_bounds equal to bounds", {

    lower_bounds <- c(1, 3,  4,   6)
    upper_bounds <- c(3, 4,  6, Inf)
    counts       <- c(3, 0, 10,  10)
    breaks       <- c(0, 1,  2,   6)
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
    # check no need to specify population bounds
    current2 <- reaggregate_counts(
        bounds = lower_bounds,
        counts = counts,
        new_bounds = breaks,
        population_weights = c(1, 3, 4, 94)
    )
    expect_equal(current, current2)
})


test_that("reaggregate_count matches Edwins for example with weights", {

    skip_if_not_installed("dplyr")

    bounds <- c(0, 80, 150, 180)
    counts <- c(10, 20, 30, 40)
    new_bounds <- c(0, 60, 150, 160, 180)
    population_bounds <- c(0, 60, 150, 160, 175, 180)
    population_weights <- c(10, 20, 30, 40, 50, 60)

    current <- reaggregate_counts(
        bounds = bounds,
        counts = counts,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    target <- reaggregate_counts_edwin_weighted(
        bounds = bounds,
        counts = counts,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    current <- current[-c(1,3)]
    expect_equal(
        current,
        setNames(target, names(current))
    )
})

