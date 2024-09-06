test_that("reaggregate_interval_rates works", {

    .reaggregate_rate_check <- function(rate, from, to, population_vector) {
        stopifnot(unique(from) == from)
        lb <- seq_along(population_vector) - 1
        ub <- c(lb[-1L], Inf)
        n_from <- suppressWarnings(
            reaggregate_interval_counts(
                breaks = from,
                lower_bounds = lb,
                upper_bounds = ub,
                max_upper = length(population_vector),
                counts = population_vector
            )
        )
        n_from <- n_from[!is.na(n_from$interval), ]
        n_to <- suppressWarnings(
            reaggregate_interval_counts(
                breaks = to,
                lower_bounds = lb,
                upper_bounds = ub,
                max_upper = length(population_vector),
                counts = population_vector
            )
        )
        n_to <- n_to[!is.na(n_to$interval), ]

        x <- suppressWarnings(
            ageutils::reaggregate_interval_counts(
                breaks = to,
                lower_bounds = from,
                upper_bounds = c(from[-1L], Inf),
                max_upper = length(population_vector),
                counts = rate * n_from$count,
                weights = population_vector
            )
        )
        x <- x[!is.na(x$interval), ]
        x$rate <- x$count / n_to$count
        x["count"] <- NULL
        x
    }

    rate <- c(1, 0.1, 0.01)
    from <- c(0, 5, 15)
    to <- c(0, 1, 9, 15, 25)
    population_vector <- round(runif(70, 10, 30))
    expected <- .reaggregate_rate_check(rate, from, to, population_vector)

    lower <- from
    upper <- c(from[-1L], Inf)
    out <- suppressWarnings(
        reaggregate_interval_rates(
            lower_bounds = lower,
            upper_bounds = upper,
            breaks = to,
            rates = rate,
            weights = population_vector
        )
    )
    expect_equal(out, expected)

    # Regression test for https://github.com/TimTaylor/ageutils/issues/8
    lower <- seq.int(65L, 90L, 5L)
    upper <- lower + 5L
    rates <- c(7.1, 5.7, 4.9, 4.2, 3.6, 2.9)
    breaks <- 65:95
    expected <- data.frame(
        lower_bound = 65:95,
        upper_bound = c(66:95, Inf),
        rate = c(rep(rates, each = 5L), 0)
    )

    out <- suppressWarnings(
        reaggregate_interval_rates(
            lower_bounds = lower,
            upper_bounds = upper,
            rates = rates,
            breaks = breaks
        )
    )

    expected <- data.frame(
        lower_bound = 65:95,
        upper_bound = c(66:95, Inf),
        rate = c(rep(rates, each = 5L), 0)
    )

    expect_equal(out[-1L], expected)

    expect_warning(
        reaggregate_interval_rates(
            lower_bounds = lower,
            upper_bounds = upper,
            rates = rates,
            breaks = breaks
        ),
        class = "deprecatedWarning"
    )
})

test_that("reaggregate_interval_counts work", {

    # without weights
    lower_bounds <- c(1, 2, 1,        4,   6)
    upper_bounds <- c(3, 3, 3,        6, Inf)
    counts       <- c(1, 1, 1, NA_real_,  10)
    max_upper <- 100
    breaks <- c(0, 1, 2, 6)

    expected <- data.frame(
        interval = factor(
            c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)"),
            levels = c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)"),
            ordered = TRUE
        ),
        lower_bound = c(0, 1, 2, 6),
        upper_bound = c(1, 2, 6, Inf),
        count = c(0, 1, NA, 10)
    )

    expect_equal(
        suppressWarnings(
            reaggregate_interval_counts(
                lower_bounds = lower_bounds,
                upper_bounds = upper_bounds,
                counts = counts,
                breaks = breaks,
                max_upper = max_upper
            )
        ),
        expected
    )

    # with weights
    weights <- c(1, 3, rep.int(1, 98))

    expected <- data.frame(
        interval = factor(
            c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)"),
            levels = c("[0, 1)", "[1, 2)", "[2, 6)", "[6, Inf)"),
            ordered = TRUE
        ),
        lower_bound = c(0, 1, 2, 6),
        upper_bound = c(1, 2, 6, Inf),
        count = c(0, 1.5, NA, 10)
    )

    expect_equal(
        suppressWarnings(
            reaggregate_interval_counts(
                lower_bounds = lower_bounds,
                upper_bounds = upper_bounds,
                counts = counts,
                breaks = breaks,
                weights = weights,
                max_upper = max_upper
            )
        ),
        expected
    )

    expect_warning(
        reaggregate_interval_counts(
            lower_bounds = lower_bounds,
            upper_bounds = upper_bounds,
            counts = counts,
            breaks = breaks,
            weights = weights,
            max_upper = max_upper
        ),
        class = "deprecatedWarning"
    )

})

test_that("aggregate_age_counts works", {

    # single break
    dat <- 1:10
    brk <- c(0, 5L)
    expected <- data.frame(
        interval = factor(c("[0, 5)", "[5, Inf)"), ordered = TRUE),
        lower_bound = c(0, 5),
        upper_bound = c(5, Inf),
        count = c(15, 40)
    )
    expect_equal(
        suppressWarnings(aggregate_age_counts(dat, breaks = brk)),
        expected
    )


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
        suppressWarnings(aggregate_age_counts(counts, ages, breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L))),
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
    expect_equal(suppressWarnings(aggregate_age_counts(counts, ages, breaks)), expected)

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
    expect_equal(suppressWarnings(aggregate_age_counts(counts, ages, breaks)), expected)

    # error messaging
    counts <- ages <- c(10, 1)
    breaks <- c(3, 10)
    expect_error(suppressWarnings(aggregate_age_counts(counts, ages, breaks)))
    expect_snapshot(
        error = TRUE,
        suppressWarnings(
            aggregate_age_counts(
                counts = c(10, 1),
                ages = c(10, 1),
                breaks = c(3, 10)
            )
        )
    )

    counts <- ages <- c(10, 1)
    breaks <- c(0, counts)
    expect_error(suppressWarnings(aggregate_age_counts(counts, ages, breaks)))
    expect_snapshot(
        error = TRUE,
        suppressWarnings(
            aggregate_age_counts(
                counts = c(10, 1),
                ages = c(10, 1),
                breaks = c(0, 10, 1)
            )
        )
    )

    expect_error(
        suppressWarnings(
            aggregate_age_counts(
                counts = 1:10,
                ages = as.character(1:10),
                breaks = 5L
            )
        )
    )

    expect_snapshot(
        error = TRUE,
        suppressWarnings(
            aggregate_age_counts(
                counts = 1:10,
                ages = as.character(1:10),
                breaks = 5L
            )
        )
    )

    expect_error(suppressWarnings(aggregate_age_counts(1:10, 6:14, 5L)))
    expect_snapshot(error = TRUE, suppressWarnings(aggregate_age_counts(1:10, 6:14, 5L)))

    expect_error(suppressWarnings(aggregate_age_counts("bob", breaks = 1L)))
    expect_snapshot(error = TRUE, suppressWarnings(aggregate_age_counts("bob", breaks = 1L)))

    expect_error(suppressWarnings(aggregate_age_counts(1:10, breaks = NA_integer_)))
    expect_snapshot(error = TRUE, suppressWarnings(aggregate_age_counts(1:10, breaks = NA_integer_)))

    expect_error(suppressWarnings(aggregate_age_counts(1:10, breaks = c(2L, 2L))))
    expect_snapshot(error = TRUE, suppressWarnings(aggregate_age_counts(1:10, breaks = c(2L, 2L))))

    expect_error(suppressWarnings(aggregate_age_counts(1:10, breaks = "5")))
    expect_snapshot(error = TRUE, suppressWarnings(aggregate_age_counts(1:10, breaks = "5")))

    expect_warning(
        aggregate_age_counts(1:10, rep.int(NA_integer_, 10L), breaks = 2L),
        class = "deprecatedWarning"
    )

})

test_that("split_interval_counts works", {
    # without weights
    result_1 <- suppressWarnings(split_interval_counts(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_)
    ))

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
        split_interval_counts_r(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_)
        )
    )

    # with weights
    result_2 <- suppressWarnings(split_interval_counts(
        lower_bounds = c(1, 2, 1, 4),
        upper_bounds = c(3, 3, 3, 6),
        counts =       c(1, 1, 1, NA_real_),
        weights = c(1, 3, 1, 1, 1, 1),
        max_upper = 6
    ))

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
        split_interval_counts_r(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_),
            weights = c(1, 3, 1, 1, 1, 1),
            max_upper = 6
        )
    )

    # zero weights
    result_3 <- suppressWarnings(split_interval_counts(
        lower_bounds = c(1L, 6L),
        upper_bounds = c(6L, 11L),
        counts = c(5L, 5L),
        max_upper = 11,
        weights = c(rep.int(1L, 6L), rep.int(0L, 5L))
    ))
    expected_3 <- data.frame(
        age = 1:10,
        count = c(rep.int(1L, 5L), rep.int(0L, 5L))
    )
    expect_equal(result_3, expected_3)
    expect_identical(
        result_3,
        split_interval_counts_r(
            lower_bounds = c(1L, 6L),
            upper_bounds = c(6L, 11L),
            counts = c(5L, 5L),
            max_upper = 11,
            weights = c(rep.int(1L, 6L), rep.int(0L, 5L))
        )
    )


    # input checking
    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = as.character(c(1, 2, NA,  2, 1, NA, 4)),
            upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
            counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
        )),
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
            upper_bounds = as.character(c(3, 3,  1, NA, 3, NA, 6)),
            counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
        ))
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
            upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
            counts =       as.character(c(1, 1,  1,  1, 1,  1, NA_real_))
        ))
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
            upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
            counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
            max_upper = "100"
        ))
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
            upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
            counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
            max_upper = 100:101
        ))
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, 1, 7),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_)
        ),
        "`lower_bounds` must be less than `upper_bounds`.",
        fixed = TRUE
    ))

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, NA,  2, 1, NA),
            upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
            counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
        )),
        "`lower_bounds` and `upper_bounds` must be the same length.",
        fixed = TRUE
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
            upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
            counts =       c(1, 1,  1,  1, 1,  1)
        )),
        "`bounds` and `counts` must be the same length.",
        fixed = TRUE
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_),
            max_upper = 50,
            weights = 201
        )),
        "`weights` must be a vector of length 50 (`max_upper`) representing ages 0:49",
        fixed = TRUE
    )

    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_),
            weights = ""
        ))
    )

    max_upper <- 50L
    weights <- seq_len(max_upper)
    weights[1] <- - weights[1]
    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_),
            max_upper = max_upper,
            weights = weights
        ))
    )

    weights[1] <- NA_integer_
    expect_error(
        suppressWarnings(split_interval_counts(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_),
            max_upper = max_upper,
            weights = weights
        ))
    )

    # check success
    expect_snapshot(
        {dat <- split_interval_counts(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, Inf),
            counts =       c(1, 1, 1, NA_real_)
        )}
    )

    # check deprecation
    expect_warning(
        split_interval_counts(
            lower_bounds = c(1, 2, 1, 4),
            upper_bounds = c(3, 3, 3, 6),
            counts =       c(1, 1, 1, NA_real_)
        ),
        class = "deprecatedWarning"
    )


})
