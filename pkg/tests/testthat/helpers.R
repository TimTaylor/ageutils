reaggregate_counts_edwin_unweighted <- function(bounds, counts, new_bounds) {
    # bounds <- c(0, 80, 150, 180)
    # counts <- c(10, 20, 30, 40)
    # new_bounds <- c(0, 60, 150, 160, 180)
    # stop()
    # As far as I can tell we need this test! Note this is not in the current reaggregate_rates?
    stopifnot(max(bounds) >= max(new_bounds))
    tibble::tibble(lower = bounds, counts = counts) -> dat

    all_lower <- sort(unique(c(bounds, new_bounds)))
    cut_ages(all_lower, breaks = bounds) |>
        dplyr::left_join(dat, by = dplyr::join_by(lower)) |>
        dplyr::mutate(all_lower = all_lower, fraction = (dplyr::lead(all_lower) - all_lower) / (upper - lower)) |>
        # Last age group
        dplyr::mutate(fraction = ifelse(is.na(fraction), 1, fraction)) |>
        dplyr::mutate(count = fraction * counts) -> dat

    dat |>
        dplyr::select(all_lower, count) -> dat0

    cut_ages(all_lower, breaks = new_bounds) |>
        dplyr::mutate(count = dat0$count) |>
        dplyr::summarise(count = sum(count), .by = c(lower))
}

reaggregate_counts_edwin_weighted <- function(bounds, counts, new_bounds, population_bounds, population_weights) {
    # bounds <- c(0, 80, 150, 180)
    # counts <- c(10, 20, 30, 40)
    # new_bounds <- c(0, 60, 150, 160, 180)
    # population_bounds <- c(0, 60, 150, 160, 175, 180)
    # population_weights <- c(10, 20, 30, 40, 50, 60)
    # library(tidyverse)
    # stop()

    all_lower <- sort(unique(c(bounds, new_bounds, population_bounds)))
    cut_ages(all_lower, breaks = bounds) -> dat10
    dat10 |>
        dplyr::left_join(
            tibble::tibble(lower = bounds, count = counts),
            by = dplyr::join_by(lower)) -> dat0

    dat1 <- reaggregate_counts_edwin_unweighted(population_bounds, population_weights, all_lower)
    # c_k = c_i N_k/N_i, where i is old bounds, k is new bounds
    cut_ages(all_lower, breaks = new_bounds) -> dat3
    dat0 |>
        dplyr::mutate(lower = all_lower) |>
        dplyr::left_join(dplyr::rename(dat1, w = count), by = dplyr::join_by(lower)) |>
        dplyr::mutate(i = dat0$lower) |>
        dplyr::mutate(ck = count * w/sum(w), .by = c(i)) |>
        dplyr::mutate(lower = dat3$lower) |>
        dplyr::summarise(count = sum(ck), .by = c(lower)) -> dat5

    stopifnot(isTRUE(all.equal(sum(dat5$count), sum(counts))))
    dat5
}

reaggregate_rates_edwin_weighted <- function(bounds, rates, new_bounds, population_bounds, population_weights) {
    # bounds = c(0, 80, 150, 180)
    # rates = c(0.1, 0.2, 0.3, .4)
    # new_bounds = c(0, 60, 150, 160, 180)
    # population_bounds = c(0, 60, 150, 160, 175, 180)
    # population_weights = c(10, 20, 30, 40, 50, 60)
    # stop()
    # NOTE: For Tim: I don't think we need a test here, because we can assume the rate is the same for all above the max(bounds) even if max(new_bounds) > max(bounds)
    # Instead we do need the test that max(population_bounds) < max(new_bounds). Note that I am not checking that, because that will be checked by
    # reaggregate_counts_edwin_unweighted
    tibble::tibble(lower = bounds, rates = rates) -> dat
    all_lower <- sort(unique(c(bounds, new_bounds, population_bounds)))
    cut_ages(all_lower, breaks = bounds) |>
        dplyr::left_join(dat, by = dplyr::join_by(lower)) -> dat1
    dat2 <- reaggregate_counts_edwin_unweighted(population_bounds, population_weights, all_lower)
    dat1 |>
        dplyr::mutate(weight = dat2$count) -> dat3
    cut_ages(all_lower, breaks = new_bounds) -> dat4
    dat3 |>
        dplyr::mutate(lower = dat4$lower) |>
        dplyr::summarise(rate = sum(rates * weight) / sum(weight), .by = c(lower))
}

reaggregate_counts_fast <- function(
    bounds,
    counts,
    new_bounds,
    ...,
    population_bounds = NULL,
    population_weights = NULL
) {

    check_dots_empty0(...)

    # lower bounds checks
    if (any(!is.finite(bounds)))
        stop("`bounds` must be a finite, numeric vector.")

    if (!length(bounds))
        stop("`bounds` must be of non-zero length.")

    if (is.unsorted(bounds, na.rm = FALSE, strictly = TRUE))
        stop("`bounds` must be in strictly ascending order")

    if (bounds[1L] < 0)
        stop("`bounds` must be non-negative.")

    # rates checks
    if(!is.numeric(counts))
        stop("`counts` must be numeric.")

    if (length(counts) != length(bounds))
        stop("`counts` must be the same length as `bounds`.")

    # new bounds checks
    if (any(!is.finite(new_bounds)))
        stop("`new_bounds` must be a finite, numeric vector.")

    if (!length(new_bounds))
        stop("`new_bounds` must be of non-zero length.")

    if (is.unsorted(new_bounds, na.rm = FALSE, strictly = TRUE))
        stop("`new_bounds` must be in strictly ascending order")

    if (new_bounds[1L] < 0)
        stop("`new_bounds` must be non-negative.")

    # population bounds checks
    if (is.null(population_bounds)) {

        if (!is.null(population_weights)) {
            if (length(population_weights) != length(new_bounds)) {
                stop("When `population_bounds` is not specified, `population_weights` must be the same length as `new_bounds`."
                )
            }
        }

        if (max(bounds) < max(new_bounds)) {
            stop("Where `population_bounds` are not specified the maximum value of `new_bounds` must be less than or equal to that of `bounds`."
            )
        }

        population_bounds <- new_bounds

    } else {

        if (any(!is.finite(population_bounds)))
            stop("`population_bounds` must be a finite, numeric vector.")

        if (!length(population_bounds))
            stop("`population_bounds` must be of non-zero length.")

        if (is.unsorted(population_bounds, na.rm = FALSE, strictly = TRUE))
            stop("`population_bounds` must be in strictly ascending order")

        if (population_bounds[1L] < 0)
            stop("`population_bounds} must be non-negative.")

        if (max(bounds) > max(population_bounds)) {
            stop(
                "The maximum value of `bounds` must be less than or equal to that of `population_bounds`."
            )
        }

    }

    # population_weights check
    if (!is.null(population_weights)) {

        if (any(!is.finite(population_weights)) || any(population_weights < 0))
            stop("`population_weights` must be numeric, non-negative and finite.")

        if (length(population_weights) != length(population_bounds))
            stop("`population_weights` must be the same length as `population_bounds`."
            )

        if (sum(population_weights) == 0)
            stop("At least one `population_weight` must be non-zero.")
    }

    # Ensure bounds start at zero and adjust counts accordingly
    if (bounds[1L] != 0) {
        bounds <- c(0, bounds)
        counts <- c(0, counts)
    }

    # Ensure new bounds start at zero
    if (new_bounds[1L] != 0)
        new_bounds <- c(0, new_bounds)

    # Ensure population_bounds start at zero and adjust weights accordingly
    if (population_bounds[1L] != 0) {
        population_bounds <- c(0, population_bounds)
        if (!is.null(population_weights))
            population_weights <- c(0, population_weights)
    }

    # calculate the old and new upper bounds
    old_upper <- c(bounds[-1L], Inf)
    pop_upper <- c(population_bounds[-1L], Inf)
    new_upper <- c(new_bounds[-1L], Inf)

    # calculate the combined bounds
    all_lower <- sort(unique(c(bounds, new_bounds, population_bounds)))
    all_upper <- c(all_lower[-1L], Inf)

    if (is.null(population_weights))
        population_weights <- pop_upper - population_bounds

    # we need to keep track where the combined bits would fit in the old and
    # new bounds. This information is stored in the old_container and
    # new_container vectors respectively.
    new_container <- old_container <- pop_container <- integer(length(all_upper))
    new_index <- old_index <- pop_index <- 1L

    for (i in seq_along(old_container)) {
        old_index <- old_index + (all_upper[i] > old_upper[old_index])
        new_index <- new_index + (all_upper[i] > new_upper[new_index])
        pop_index <- pop_index + (all_upper[i] > pop_upper[pop_index])

        old_container[i] <- old_index
        new_container[i] <- new_index
        pop_container[i] <- pop_index
    }

    result <- counts[old_container]

    all_diff <- all_upper - all_lower
    pop_diff <- (pop_upper[pop_container] - population_bounds[pop_container])
    ratio <- all_diff / pop_diff
    ratio[all_diff == Inf & pop_diff == Inf] <- 1
    pop_weights <- population_weights[pop_container] * ratio
    pop_weights <- pop_weights / ave(pop_weights, old_container, FUN = sum)
    result <- counts[old_container] * pop_weights
    result[length(result)] <- sum(counts) - sum(result[-length(result)])

    out <- numeric(length(new_bounds))
    idx <- 1L
    for (i in seq_along(new_container)) {
        if (new_container[i] != idx)
            idx <- idx + 1L
        out[idx] <- out[idx] + result[i]
    }
    out[idx] <- sum(counts) - sum(out[-idx])

    interval <- sprintf("[%.f, %.f)", new_bounds, new_upper)
    interval <- factor(interval, levels = interval, ordered = TRUE)

    new_tibble(
        list(
            interval = interval,
            lower = new_bounds,
            upper = new_upper,
            count = out
        )
    )

}

reaggregate_rates_fast <- function(
        bounds,
        rates,
        new_bounds,
        ...,
        population_bounds = NULL,
        population_weights = NULL
) {

    check_dots_empty0(...)

    # lower bounds checks
    if (any(!is.finite(bounds)))
        stop("`bounds` must be a finite, numeric vector.")
    if (!length(bounds))
        stop("`bounds` must be of non-zero length.")
    if (is.unsorted(bounds, na.rm = FALSE, strictly = TRUE))
        stop("`bounds` must be in strictly ascending order")
    if (bounds[1L] < 0)
        stop("`bounds` must be non-negative.")

    # rates checks
    if(!is.numeric(rates))
        stop("`rates` must be numeric.")
    if (length(rates) != length(bounds))
        stop("`rates` must be the same length as `bounds`.")

    # new bounds checks
    if (any(!is.finite(new_bounds)))
        stop("`new_bounds` must be a finite, numeric vector.")
    if (!length(new_bounds))
        stop("`new_bounds` must be of non-zero length.")
    if (is.unsorted(new_bounds, na.rm = FALSE, strictly = TRUE))
        stop("`new_bounds` must be in strictly ascending order")
    if (new_bounds[1L] < 0)
        stop("`new_bounds` must be non-negative.")

    # population bounds checks
    if (is.null(population_bounds)) {

        if (!is.null(population_weights)) {
            if (length(population_weights) != length(new_bounds)) {
                stop("When `population_bounds` is not specified, `population_weights` must be the same length as `new_bounds`.")
            }
        }

        if (max(bounds) < max(new_bounds)) {
            stop("Where `population_bounds` are not specified the maximum value of `new_bounds` must be less than or equal to that of `bounds`."
            )
        }

        population_bounds <- new_bounds

    } else {

        if (any(!is.finite(population_bounds)))
            stop("`population_bounds` must be a finite, numeric vector.")

        if (!length(population_bounds))
            stop("`population_bounds` must be of non-zero length.")

        if (is.unsorted(population_bounds, na.rm = FALSE, strictly = TRUE))
            stop("`population_bounds` must be in strictly ascending order")

        if (population_bounds[1L] < 0)
            stop("`population_bounds` must be non-negative.")

        if (max(population_bounds) < max(new_bounds)) {
            stop("The maximum value of `new_bounds` must be less than or equal to that of `population_bounds`.")
        }

    }

    # population_weights check
    if (!is.null(population_weights)) {
        if (any(!is.finite(population_weights)) || any(population_weights < 0))
            stop("`population_weights` must be numeric, non-negative and finite.")
        if (length(population_weights) != length(population_bounds))
            stop("`population_weights` must be the same length as `population_bounds`.")
        if (sum(population_weights) == 0)
            stop("At least one `population_weight` must be non-zero.")
    }

    # Ensure bounds start at zero and adjust rates accordingly
    if (bounds[1L] != 0) {
        bounds <- c(0, bounds)
        rates <- c(0, rates)
    }

    # Ensure new bounds start at zero
    if (new_bounds[1L] != 0)
        new_bounds <- c(0, new_bounds)

    # Ensure population_bounds start at zero and adjust weights accordingly
    if (population_bounds[1L] != 0) {
        population_bounds <- c(0, population_bounds)
        if (!is.null(population_weights))
            population_weights <- c(0, population_weights)
    }

    # calculate the old and new upper bounds
    old_upper <- c(bounds[-1L], Inf)
    pop_upper <- c(population_bounds[-1L], Inf)
    new_upper <- c(new_bounds[-1L], Inf)

    # calculate the combined bounds
    all_lower <- sort(unique(c(bounds, new_bounds, population_bounds)))
    all_upper <- c(all_lower[-1L], Inf)

    # TODO - explain this!!!
    if (is.null(population_weights)) {
        population_weights <- pop_upper - population_bounds
        population_weights[length(population_weights)] <- 1
    }

    # we need to keep track where the combined bits would fit in the old and
    # new bounds. This information is stored in the old_container and
    # new_container vectors respectively.
    new_container <- old_container <- pop_container <- integer(length(all_upper))
    new_index <- old_index <- pop_index <- 1L

    for (i in seq_along(old_container)) {
        old_index <- old_index + (all_upper[i] > old_upper[old_index])
        new_index <- new_index + (all_upper[i] > new_upper[new_index])
        pop_index <- pop_index + (all_upper[i] > pop_upper[pop_index])

        old_container[i] <- old_index
        new_container[i] <- new_index
        pop_container[i] <- pop_index
    }

    all_diff <- all_upper - all_lower
    pop_diff <- (pop_upper[pop_container] - population_bounds[pop_container])
    ratio <- all_diff / pop_diff
    ratio[all_diff == Inf & pop_diff == Inf] <- 1
    pop_weights <- population_weights[pop_container] * ratio

    result <- rates[old_container] * pop_weights
    out <- numeric(length(new_bounds))
    idx <- 1L
    weight <- 0
    for (i in seq_along(new_container)) {
        if (new_container[i] != idx) {
            out[idx] <- out[idx] / weight
            idx <- idx + 1L
            weight <- 0
        }
        weight <- weight + pop_weights[i]
        out[idx] <- out[idx] + result[i]
    }
    out[length(out)] <- out[length(out)] / weight

    interval <- sprintf("[%.f, %.f)", new_bounds, new_upper)
    interval <- factor(interval, levels = interval, ordered = TRUE)

    new_tibble(
        list(
            interval = interval,
            lower = new_bounds,
            upper = new_upper,
            rate = out
        )
    )

}
