# -------------------------------------------------------------------------
#' Reaggregate rates across intervals
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `reaggregate_interval_rates()` enables the reweighting of interval rates in
#' to different intervals ranges. It first replicates the rates of a given
#' age interval into the individual years of said interval. These are then
#' aggregated allowing for a user specified weight vector.
#'
# -------------------------------------------------------------------------
#' @param lower_bounds,upper_bounds `[integerish]`.
#'
#' A pair of vectors representing the bounds of the current intervals.
#'
#' If `upper_bounds` is NULL, it will be automatically set to
#' `c(lower_bounds[-1L], Inf)`.
#'
#' `lower_bounds` must be strictly less than `upper_bounds` and greater than or
#' equal to zero.
#'
#' Missing (NA) bounds are not permitted.
#'
#' Double vectors will be coerced to integer.
#'
#' @param rates `[numeric]`.
#'
#' Vector of counts to be averaged.
#'
#' @param breaks `[numeric]`.
#'
#' 1 or more non-negative cut points in increasing (strictly) order.
#'
#' These correspond to the left hand side of the desired intervals (e.g. the
#' closed side of [x, y).
#'
#' Double values are coerced to integer prior to categorisation.
#'
#' @param weights `[numeric]`
#'
#' Population weightings to apply for individual years.
#'
#' If `NULL` (default) weights will be allocated proportional to the interval
#' size.
#'
#' If specified, must be of length most 2000 and represent weights in the
#' range 0:1999.
#'
#' `weights` of length less than 2000 will be padded with 0.
#'
# -------------------------------------------------------------------------
#' @return
#' A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and an
#' associated `count`.
#'
# -------------------------------------------------------------------------
#' @examples
#' reaggregate_interval_rates(
#'   lower_bounds = c(0, 5, 13),
#'   upper_bounds= c(5, 15, 100),
#'   rates = c(1, 0.1, 0.01),
#'   breaks = c(0, 1, 9, 15),
#'   weights = round(runif(70, 10, 30))
#' )
#'
#' reaggregate_interval_rates(
#'   lower_bounds = c(0, 5, 13),
#'   rates = c(1, 0.1, 0.01),
#'   breaks = c(0, 1, 9, 15),
#'   weights = round(runif(70, 10, 30))
#' )
#'
#'
# -------------------------------------------------------------------------
#' @export
reaggregate_interval_rates <- function(
    lower_bounds,
    upper_bounds = NULL,
    rates,
    breaks,
    weights = NULL
){

    # For consistency with other C implementations set a MAXBOUND
    .MAXBOUND = 2000L

    # ensure numeric input that isn't NA
    if (!is.numeric(lower_bounds) || anyNA(lower_bounds))
        stop("`lower_bounds` must be numeric and not NA.")
    if (!is.null(upper_bounds)) {
        if (!is.numeric(upper_bounds) || anyNA(upper_bounds)) {
            stop("`upper_bounds` must be numeric and not NA.")
        }
    }
    if (!is.numeric(rates) || anyNA(rates))
        stop("`rates` must be numeric and not NA.")
    if (!is.numeric(breaks) || anyNA(breaks))
        stop("`breaks` must be numeric and not NA.")
    if (!is.null(weights)) {
        if(!is.numeric(weights) || anyNA(weights)) {
            stop("`weights` must be numeric and not NA.")
        }
    }

    # check bounds and rates have compatible lengths
    n_bounds <- length(lower_bounds)
    if (is.null(upper_bounds)) {
        upper_bounds <- c(lower_bounds[-1L], Inf)
    }

    n_upper_bounds <- length(upper_bounds)
    if (n_bounds != n_upper_bounds)
        stop("`lower_bounds` and `upper_bounds` must be the same length.")

    n_rates <- length(rates)
    if  (n_bounds != n_rates)
        stop("`lower_bounds` and `rates` must be the same length.")

    # Check bounds are in strictly increasing order and non-negative
    if (is.unsorted(lower_bounds, strictly = TRUE) || any(lower_bounds < 0))
        stop("`lower_bounds` must be non-negative and in strictly increasing order.");

    if (is.unsorted(upper_bounds, strictly = TRUE) || any(upper_bounds < 0))
        stop("`upper_bounds` must be non-negative and in strictly increasing order.");

    # Ensure lower bounds are less than upper bounds
    if(any(upper_bounds < lower_bounds))
        stop("`lower_bounds` must be less than `upper_bounds`.")

    # Ensure reasonable upper bound
    top_bound <- upper_bounds[n_bounds]
    finite_top <- is.finite(top_bound)
    if (finite_top && top_bound >= .MAXBOUND) {
        stopf("unless infinite, `upper_bounds` must be less than %d.", .MAXBOUND)
    } else if (!finite_top) {
        upper_bounds[n_bounds] <- .MAXBOUND
        top_bound <- .MAXBOUND
    }

    # Ensure valid weights and extend to cover length .MAXBOUND
    if (is.null(weights)) {
        weights <- numeric(length = .MAXBOUND) + 1L
    } else {
        n_weights <- length(weights)
        if (n_weights == 0L)
            stop("Zero length `weights` not permitted")
        if (n_weights > .MAXBOUND)
            stopf("`weights` must be a vector of length %d at most.", .MAXBOUND)
        if (n_weights < .MAXBOUND) {
            tmp <- numeric(length = .MAXBOUND)
            tmp[seq_len(n_weights)] <- weights
            weights <- tmp
        }
    }

    if (any(weights < 0))
        stop("`weights` must be positive.")

    # check breaks
    if (is.unsorted(breaks, strictly = TRUE) || any(breaks < 0))
        stop("`breaks` must be non-negative and in strictly increasing order.");
    n_breaks <- length(breaks)
    if (breaks[n_breaks] >= .MAXBOUND)
        stopf("`breaks` must all be less than %d.", .MAXBOUND)

    # coerce bounds to integer
    lower_bounds <- as.integer(lower_bounds)
    upper_bounds <- as.integer(upper_bounds)

    # Expand and rates across ages
    age_rates <- numeric(length = .MAXBOUND)
    for (i in seq_along(rates)) {
        idx <- lower_bounds[i]:(upper_bounds[i] - 1L)
        age_rates[idx + 1L] <- rates[i]
    }

    # coerce breaks to integer
    breaks <- as.integer(breaks)
    breaks <- c(breaks, .MAXBOUND)
    n_breaks <- n_breaks + 1L

    # calculate the aggregate rates
    group_rates <- numeric(n_breaks - 1L)
    group_totals <- integer(n_breaks - 1L)
    group_index <- 1L

    for (i in seq.int(from = breaks[1L] + 1L, to = .MAXBOUND, by = 1L)) {
        current_age <- i - 1L
        while(group_index < n_breaks && current_age >= breaks[group_index + 1L]) {
            group_index <- group_index + 1L
        }
        group_rates[group_index] <- group_rates[group_index] + (age_rates[i] * weights[i])
        group_totals[group_index] <- group_totals[group_index] + weights[i]
    }

    set_to_zero <- group_totals == 0
    rates_out <- group_rates / group_totals
    rates_out[set_to_zero] <- 0

    # calculate returned intervals
    lower_out <- breaks[-n_breaks]
    upper_out <- breaks[-1L]
    intervals <- sprintf("[%d, %d)", lower_out, upper_out)
    upper_out[n_breaks - 1] <- Inf
    intervals[n_breaks - 1] <- sprintf("[%d, Inf)", lower_out[n_breaks - 1])
    intervals <- factor(intervals, levels = intervals, ordered = TRUE)

    # return as data frame
    list2DF(
        list(
            interval = intervals,
            lower_bound = as.numeric(lower_out),
            upper_bound = upper_out,
            rate = rates_out
        )
    )
}
