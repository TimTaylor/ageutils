# -------------------------------------------------------------------------
#' Reaggregate age rates
#'
# -------------------------------------------------------------------------
#' `reaggregate_rates()` converts rates over one interval range to another
#' with optional weighting by a known population.
#'
# -------------------------------------------------------------------------
#' @param bounds `[numeric]`
#'
#' The *current* boundaries in (strictly) increasing order.
#'
#' These correspond to the left hand side of the intervals (e.g. the
#' closed side of [x, y).
#'
#' Double values are coerced to integer prior to categorisation.
#'
#' @param rates `[numeric]`
#'
#' Vector of rates corresponding to the intervals defined by `bounds`.
#'
#' @param new_bounds `[numeric]`
#'
#' The *desired* boundaries in (strictly) increasing order.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @param population_bounds  `[numeric]`
#'
#' Interval boundaries for a known population weighting given by the
#' `population_weights` argument.
#'
#' @param population_weights `[numeric]`
#'
#' Population weightings corresponding to `population_bounds`.
#'
#' Used to weight the output across the desired intervals.
#'
#' If `NULL` (default) rates are divided proportional to the interval sizes.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and a
#' corresponding `rate`.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' reaggregate_rates(
#'     bounds = c(0, 5, 10),
#'     rates = c(0.1, 0.2 ,0.3),
#'     new_bounds = c(0, 2, 7, 10),
#'     population_bounds = c(0, 2, 5, 7, 10),
#'     population_weights = c(100, 200, 50, 150, 100)
#' )
#'
# -------------------------------------------------------------------------
#' @export
reaggregate_rates <- function(...) {
    UseMethod("reaggregate_rates")
}

#' @rdname reaggregate_rates
#'@export
reaggregate_rates.default <- function(
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
        cli_abort("{.arg bounds} must be a finite, numeric vector.")
    if (!length(bounds))
        cli_abort("{.arg bounds} must be of non-zero length.")
    if (is.unsorted(bounds, na.rm = FALSE, strictly = TRUE))
        cli_abort("{.arg bounds} must be in strictly ascending order")
    if (bounds[1L] < 0)
        cli_abort("{.arg bounds} must be non-negative.")

    # rates checks
    if(!is.numeric(rates))
        cli_abort("{.arg rates} must be numeric.")
    if (length(rates) != length(bounds))
        cli_abort("{.arg rates} must be the same length as `bounds`.")

    # new bounds checks
    if (any(!is.finite(new_bounds)))
        cli_abort("{.arg new_bounds} must be a finite, numeric vector.")
    if (!length(new_bounds))
        cli_abort("{.arg new_bounds} must be of non-zero length.")
    if (is.unsorted(new_bounds, na.rm = FALSE, strictly = TRUE))
        cli_abort("{.arg new_bounds} must be in strictly ascending order")
    if (new_bounds[1L] < 0)
        cli_abort("{.arg new_bounds} must be non-negative.")

    # population bounds checks
    if (is.null(population_bounds)) {

        if (!is.null(population_weights)) {
            if (length(population_weights) != length(new_bounds)) {
                cli_abort(
                    "When {.arg population_bounds} is not specified, {.arg population_weights}
                     must be the same length as {.arg new_bounds}."
                )
            }
        }

        if (max(bounds) < max(new_bounds)) {
            cli_abort(
                "Where {.arg population_bounds} are not specified the maximum value of
                {.arg new_bounds} must be less than or equal to that of {.arg bounds}."
            )
        }

        population_bounds <- new_bounds

    } else {

        if (any(!is.finite(population_bounds)))
            cli_abort("{.arg population_bounds} must be a finite, numeric vector.")

        if (!length(population_bounds))
            cli_abort("{.arg population_bounds} must be of non-zero length.")

        if (is.unsorted(population_bounds, na.rm = FALSE, strictly = TRUE))
            cli_abort("{.arg population_bounds} must be in strictly ascending order")

        if (population_bounds[1L] < 0)
            cli_abort("{.arg population_bounds} must be non-negative.")

        if (max(population_bounds) < max(new_bounds)) {
            cli_abort(
                "{.arg new_bounds} must be less than or equal to that of {.arg population bounds}."
            )
        }

    }

    # population_weights check
    if (!is.null(population_weights)) {
        if (any(!is.finite(population_weights)) || any(population_weights < 0))
            cli_abort("{.arg population_weights} must be numeric, non-negative and finite.")
        if (length(population_weights) != length(population_bounds))
            cli_abort("{.arg population_weights} must be the same length as `population_bounds`.")
        if (sum(population_weights) == 0)
            cli_abort("At least one {.arg population_weight} must be non-zero.")
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

    pop_weights <- population_weights[pop_container]
    pop_weights <- pop_weights * (all_upper - all_lower) / (pop_upper[pop_container] - population_bounds[pop_container])
    pop_weights[length(pop_weights)] <- 1
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
