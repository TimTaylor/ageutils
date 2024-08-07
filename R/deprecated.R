# -------------------------------------------------------------------------
#' Deprecated functions
#'
# -------------------------------------------------------------------------
#' The functions listed below are deprecated and may be removed in a future
#' package release.
#'
# -------------------------------------------------------------------------
#' @note Help pages for each deprecated function can be found via
#' `help("<function>-deprecated")`.
#'
# -------------------------------------------------------------------------
#' @name ageutils-deprecated
#' @keywords internal
NULL



# -------------------------------------------------------------------------
#' Reaggregate rates across intervals
#'
# -------------------------------------------------------------------------
#' @description
#'
#' DEPRECATED: Please use `reaggregate_rates()` as an alternative.
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
#' @usage
#'
#' reaggregate_interval_rates(
#'     lower_bounds,
#'     upper_bounds = NULL,
#'     rates,
#'     breaks,
#'     weights = NULL
#' )
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
#' @name reaggregate_interval_rates-deprecated
#' @keywords internal
NULL

#' @rdname ageutils-deprecated
#'
#' @seealso
#' [reaggregate_rates()]:  A replacement for `reaggregate_interval_rates()`.
#'
#' @export
reaggregate_interval_rates <- function(
    lower_bounds,
    upper_bounds = NULL,
    rates,
    breaks,
    weights = NULL
){

    .Deprecated(
        new = "reaggregate_rates",
        package = "ageutils",
        msg = paste(
            "reaggregate_interval_rates() was deprecated in the 0.0.5 release",
            "of ageutils and may be removed in a future release.",
            "Going forward please use the function reaggregate_rates()."
        )
    )

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



# -------------------------------------------------------------------------
#' Reaggregate age intervals
#'
# -------------------------------------------------------------------------
#' @description
#'
#' DEPRECATED: Please use `reaggregate_counts()` as an alternative.
#'
#' `reaggregate_interval_counts()` converts counts over one interval range to
#' another. It first splits counts of a given age interval in to counts for
#' individual years based on a given weighting. These are then aggregated to the
#' desired breaks. Functionally this is equivalent to, but more efficient than,
#' a call to `split_interval_counts()` followed by `aggregate_age_counts()`.
#'
# -------------------------------------------------------------------------
#' @param breaks `[numeric]`.
#'
#' 1 or more cut points in increasing (strictly) order.
#'
#' These correspond to the left hand side of the desired intervals (e.g. the
#' closed side of [x, y).
#'
#' Double values are coerced to integer prior to categorisation.
#'
#' @param counts `[numeric]`.
#'
#' Vector of counts to be aggregated.
#'
#' @param lower_bounds,upper_bounds `[integerish]`.
#'
#' A pair of vectors representing the bounds of the intervals.
#'
#' `lower_bounds` must be strictly less than `upper_bounds` and greater than or
#' equal to zero.
#'
#' Missing (NA) bounds are not permitted.
#'
#' Double vectors will be coerced to integer.
#'
#' @param max_upper `[integerish]`
#'
#' Represents the maximum upper bounds permitted upon splitting the data.
#'
#' Any upper bound greater than this will be replaced with this value prior to
#' splitting.
#'
#' Double vectors will be coerced to integer.
#'
#' @param weights `[numeric]`
#'
#' Population weightings to apply for individual years.
#'
#' If `NULL` (default) counts will be split evenly based on interval size.
#'
#' If specified, must be of length `max_upper` and represent weights in the
#' range 0:(max_upper - 1).
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and an
#' associated `count`.
#'
# -------------------------------------------------------------------------
#' @usage
#'
#' reaggregate_interval_counts(
#'     lower_bounds,
#'     upper_bounds,
#'     counts,
#'     breaks,
#'     max_upper = 100L,
#'     weights = NULL
#' )
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' reaggregate_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, 20),
#'     counts = c(5, 10, 30),
#'     breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L)
#' )
#'
# -------------------------------------------------------------------------
#' @name reaggregate_interval_counts-deprecated
#' @keywords internal
NULL

#' @rdname ageutils-deprecated
#'
#' @seealso
#' [reaggregate_counts()]:  A replacement for `reaggregate_interval_counts()`.
#'
#' @export
reaggregate_interval_counts <- function(
    lower_bounds,
    upper_bounds,
    counts,
    breaks,
    max_upper = 100L,
    weights = NULL
) {
    .Deprecated(
        new = "reaggregate_counts",
        package = "ageutils",
        msg = paste(
            "reaggregate_interval_counts() was deprecated in the 0.0.5 release",
            "of ageutils and may be removed in a future release.",
            "Going forward please use the function reaggregate_counts()."
        )
    )

    # Temporary patch for https://github.com/TimTaylor/ageutils/issues/5
    # TODO - Improve this before 0.1 release.
    stopifnot(
        is.numeric(lower_bounds),
        is.numeric(max_upper),
        length(max_upper) == 1L
    )
    if (any(max_upper <= lower_bounds))
        stop("`max_upper` must be greater than all `lower_bounds`.")

    .Call(C_reaggregate_interval_counts, lower_bounds, upper_bounds, counts, breaks, max_upper, weights)
}



# -------------------------------------------------------------------------
#' Split interval counts
#'
# -------------------------------------------------------------------------
#' @description
#'
#' DEPRECATED.
#'
#' `split_interval_counts()` splits counts of a given age interval in to
#' counts for individual years based on a given weighting. Age intervals are
#' specified by their lower (closed) and upper (open) bounds, i.e. intervals
#' of the form [lower, upper).
#'
# -------------------------------------------------------------------------
#' @param lower_bounds,upper_bounds `[integerish]`.
#'
#' A pair of vectors representing the bounds of the intervals.
#'
#' `lower_bounds` must be strictly less than `upper_bounds` and greater than or
#' equal to zero.
#'
#' Missing (NA) bounds are not permitted.
#'
#' Double vectors will be coerced to integer.
#'
#' @param counts `[numeric]`.
#'
#' Vector of counts to be aggregated.
#'
#' @param max_upper `[integerish]`
#'
#' Represents the maximum upper bounds permitted upon splitting the data.
#'
#' Any upper bound greater than this will be replaced with this value prior to
#' splitting.
#'
#' Double vectors will be coerced to integer.
#'
#' @param weights `[numeric]`
#'
#' Population weightings to apply for individual years.
#'
#' If `NULL` (default) counts will be split evenly based on interval size.
#'
#' If specified, must be of length `max_upper` and represent weights in the
#' range 0:(max_upper - 1).
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with entries `age` (in years) and `count`.
#'
# -------------------------------------------------------------------------
#' @usage
#'
#' split_interval_counts(
#'     lower_bounds,
#'     upper_bounds,
#'     counts,
#'     max_upper = 100L,
#'     weights = NULL
#' )
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' split_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, 20),
#'     counts = c(5, 10, 30)
#' )
#'
#' split_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, Inf),
#'     counts = c(5, 10, 30),
#'     max_upper = 15
#' )
#'
#' split_interval_counts(
#'     lower_bounds = c(0, 5),
#'     upper_bounds = c(5, 10),
#'     counts = c(5, 10),
#'     max_upper =10,
#'     weights = 1:10
#' )
# -------------------------------------------------------------------------
#' @name split_interval_counts-deprecated
#' @keywords internal
NULL

#' @rdname ageutils-deprecated
#'
#' @export
split_interval_counts <- function(
    lower_bounds,
    upper_bounds,
    counts,
    max_upper = 100L,
    weights = NULL
) {
    .Deprecated(
        msg = paste(
            "split_interval_counts() was deprecated in the 0.0.5 release of",
            "ageutils and may be removed in a future release."
        )
    )
    .Call(C_split_interval_counts, lower_bounds, upper_bounds, counts, max_upper, weights)
}



# -------------------------------------------------------------------------
#' Aggregate counts across ages
#'
# -------------------------------------------------------------------------
#' @description
#'
#' DEPRECATED: `reaggregate_counts()` can be used as an alternative.
#'
#' `aggregate_age_counts()` provides aggregation of counts across ages (in
#' years). It is similar to a `cut()` and `tapply()` pattern but optimised for
#' speed over flexibility. It takes a specified set of breaks representing the
#' left hand limits of a closed open interval, i.e [x, y), and returns the
#' corresponding interval and upper bounds. The resulting intervals span from
#' the minimum break through to the maximum age. Missing values are grouped as
#' NA.
#'
# -------------------------------------------------------------------------
#' @param ages `[numeric]`.
#'
#' Vector of age in years.
#'
#' Double values are coerced to integer prior to categorisation / aggregation.
#'
#' For `aggregate_age_counts()`, these must corresponding to the `counts` entry
#' and will defaults to 0:(N-1) where `N` is the number of counts present.
#'
#' No (non-missing) age can be less than the minimum break.
#'
#' @param breaks `[numeric]`.
#'
#' 1 or more cut points in increasing (strictly) order.
#'
#' These correspond to the left hand side of the desired intervals (e.g. the
#' closed side of [x, y).
#'
#' Double values are coerced to integer prior to categorisation.
#'
#' @param counts `[numeric]`.
#'
#' Vector of counts to be aggregated.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and an
#' associated `count`.
#'
# -------------------------------------------------------------------------
#' @usage
#'
#' aggregate_age_counts(
#'     counts,
#'     ages = seq_along(counts) - 1L,
#'     breaks
#' )
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # default ages generated if only counts provided (here ages will be 0:64)
#' aggregate_age_counts(counts = 1:65, breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L))
#'
#' # NA ages are handled with their own grouping
#' ages <- 1:65
#' ages[1:44] <- NA
#' aggregate_age_counts(
#'     counts = 1:65,
#'     ages = ages,
#'     breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L)
#' )
#'
# -------------------------------------------------------------------------
#' @name aggregate_age_counts-deprecated
#' @keywords internal
NULL

#' @rdname ageutils-deprecated
#'
#' @export
aggregate_age_counts <- function(
        counts,
        ages = seq_along(counts) - 1L,
        breaks
) {

    .Deprecated(
        new = "reaggregate_counts",
        package = "ageutils",
        msg = paste(
            "aggregate_age_counts() was deprecated in the 0.0.5 release of",
            "ageutils and may be removed in a future release."
        )
    )
    .Call(C_aggregate_age_counts, counts, ages, breaks)
}



# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# -------------------------------------------------------------------------
# R implementation for testing
# -------------------------------------------------------------------------
split_interval_counts_r <- function(
    lower_bounds,
    upper_bounds,
    counts,
    max_upper = 100L,
    weights = NULL
) {
    # ensure numeric bounds, counts and weights
    if (!is.numeric(lower_bounds))
        stop("`lower_bounds` must be numeric.")
    if (!is.numeric(upper_bounds))
        stop("`upper_bounds` must be numeric.")
    if (!is.numeric(counts))
        stop("`counts` must be numeric.")
    if (!(is.numeric(max_upper) && length(max_upper) == 1L))
        stop("`max_upper` must be an integer of length 1.")

    # Ensure max_upper is coercible to integer
    max_upper <- as.integer(max_upper)
    if (anyNA(max_upper))
        stop("`max_upper` must be finite, and, coercible to integer.")

    # Ensure bounds and counts have compatible lengths
    n_bounds <- length(upper_bounds)
    if (length(lower_bounds) != n_bounds)
        stop("`lower_bounds` and `upper_bounds` must be the same length.")
    if (length(counts) != n_bounds)
        stop("`bounds` and `counts` must be the same length.")

    # Ensure lower bounds are coercible to integer
    lower_bounds <- as.integer(lower_bounds)
    if (anyNA(lower_bounds))
        stop("`lower_bounds` must be finite, non-missing (not NA) and coercible to integer.")

    # Replace upper bounds greater than max_upper then ensure coercible to integer
    if (anyNA(upper_bounds))
        stop("`upper_bounds` must be non-missing (not NA).")
    idx <- upper_bounds > max_upper
    if (isTRUE(any(idx))) {
        upper_bounds[idx] <- max_upper
        warningf(
            "`upper_bounds` greater than `max_upper` (%d) have been replaced prior to splitting.",
            max_upper
        )
    }
    upper_bounds <- as.integer(upper_bounds)

    # Ensure lower bounds less than upper bounds
    if (any(lower_bounds >= upper_bounds))
        stop("`lower_bounds` must be less than `upper_bounds`.")

    # Coerce counts to double prior to calling C function
    counts <- as.double(counts)

    # check weights if not NULL
    if (is.null(weights)) {
        weights <- rep.int(1 / max_upper, max_upper)
    } else {
        if (!is.numeric(weights))
            stop("`weights` must be numeric.")
        if (anyNA(weights) || min(weights, na.rm = TRUE) < 0)
            stop("`weights` must be non-negative and not missing (NA).")
        if (length(weights) != max_upper) {
            stopf(
                "`weights` must be a vector of length %d (`max_upper`) representing ages 0:%d",
                max_upper,
                max_upper - 1
            )
        }
        weights <- as.double(weights)
    }

    n <- length(lower_bounds)
    age_lengths <- upper_bounds - lower_bounds
    total <- sum(age_lengths)
    age <-  integer(total)
    count <-  numeric(total)
    index <- 1
    for (i in seq_len(n)) {
        interval_start <- lower_bounds[i]
        interval_end <- upper_bounds[i]
        ct <- counts[i]
        ag <- interval_start:(interval_end - 1)
        wt <- weights[ag+1]
        s <- sum(wt)
        if (abs(s) > sqrt(.Machine$double.eps))
            wt <- wt / s
        new_index <- index+age_lengths[i]
        age[index:(new_index - 1)] <- ag
        count[index:(new_index - 1)] <- ct*wt
        index <- new_index
    }

    list2DF(list(age=age, count=count))
}
