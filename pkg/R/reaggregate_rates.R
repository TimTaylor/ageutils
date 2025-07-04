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
#' A data frame with 4 entries; `interval`, `lower`, `upper` and a
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
reaggregate_rates <- function(
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
    pop_upper <- c(population_bounds[-1L], Inf)
    new_upper <- c(new_bounds[-1L], Inf)

    # TODO - explain this!!!
    if (is.null(population_weights)) {
        population_weights <- pop_upper - population_bounds
        population_weights[length(population_weights)] <- 1 # here the value is irrelevant as long as finite (I think)
    }

    # Do the stuff
    DT <- .reaggregate_rates(
        bounds = bounds,
        rates = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    # calculate the intervals
    interval <- sprintf("[%.f, %.f)", new_bounds, new_upper)
    interval <- factor(interval, levels = interval, ordered = TRUE)

    # return as tibble
    new_tibble(
        list(
            interval = interval,
            lower = new_bounds,
            upper = new_upper,
            rate = DT$rate
        )
    )
}

.reaggregate_rates <- function(bounds, rates, new_bounds, population_bounds, population_weights) {
    all_lower <- sort(unique(c(bounds, new_bounds, population_bounds)))
    # vctrs::new_data_frame should be safe to use here due to earlier input
    #   checks in the user facing function
    dat <- new_data_frame(list(lower = bounds, rates = rates))
    cut <- cut_ages(all_lower, breaks = bounds)
    dat1 <- merge(cut, dat, by = "lower")
    dat2 <- .reaggregate_counts_unweighted(population_bounds, population_weights, all_lower)
    dat1$weight <- dat2$count
    cut2 <- cut_ages(all_lower, breaks = new_bounds)
    dat1$lower <- cut2$lower

    # The following is optimised for performance for our use cases but is the
    # equivalent (save output type) of
    # setDT(dat1)[, .(rate = sum(rates * weight) / sum(weight)), by = "lower"][]
    out <- .fgsum(dat1$rates * dat1$weight, by = dat1$lower, byname = "lower", sumname = "rate")
    sw <- .fgsum(dat1$weight, by = dat1$lower, byname = "lower", sumname = "rate")
    out$rate <- out$rate / sw$rate
    out


}
