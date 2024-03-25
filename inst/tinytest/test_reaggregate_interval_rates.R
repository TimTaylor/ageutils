reaggregate_rate_check <- function(rate, from, to, population_vector) {
    stopifnot(unique(from) == from)
    lb <- seq_along(population_vector) - 1
    ub <- c(lb[-1L], Inf)
    n_from <- suppressWarnings(
        ageutils::reaggregate_interval_counts(
            breaks = from,
            lower_bounds = lb,
            upper_bounds = ub,
            max_upper = length(population_vector),
            counts = population_vector
        )
    )
    n_from <- n_from[!is.na(n_from$interval), ]
    n_to <- ageutils::reaggregate_interval_counts(
        breaks = to,
        lower_bounds = lb,
        upper_bounds = ub,
        max_upper = length(population_vector),
        counts = population_vector
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
expected <- reaggregate_rate_check(rate,from,to,population_vector)

lower <- from
upper <- c(from[-1L], Inf)
out <- reaggregate_interval_rates(
    lower_bounds = lower,
    upper_bounds = upper,
    breaks = to,
    rates = rate,
    weights = population_vector
)
expect_equal(out, expected)
