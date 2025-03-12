# -------------------------------------------------------------------------
#' Reaggregate age counts
#'
# -------------------------------------------------------------------------
#' `reaggregate_counts()` converts counts over one interval range to another
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
#' @param counts `[numeric]`
#'
#' Vector of counts corresponding to the intervals defined by `bounds`.
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
#' If `NULL` (default), counts are divided proportional to the interval sizes.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and a
#' corresponding `count`.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # Reaggregating some data obtained from the 2021 UK census
#' head(pop_dat)
#'
#' # Each row of the data is for the same region so we can drop some columns
#' # `age_category` and `value` columns
#' dat <- subset(pop_dat, select = c(age_category, value))
#'
#' # Add the lower bounds to the data
#' dat <- transform(
#'     dat,
#'     lower_bound = as.integer(sub("\\[([0-9]+), .+)", "\\1", age_category))
#' )
#'
#' # Now recategorise to the desired age intervals
#' with(
#'     dat,
#'     reaggregate_counts(
#'         bounds = lower_bound,
#'         counts = value,
#'         new_bounds = c(0, 1, 5, 15, 25, 45, 65)
#'     )
#' )
#'
# -------------------------------------------------------------------------
#' @export
reaggregate_counts <- function(...) {
    UseMethod("reaggregate_counts")
}

#' @rdname reaggregate_counts
#'@export
reaggregate_counts.default <- function(
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

        if (max(bounds) < max(population_bounds)) {
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


    pop_weights <- population_weights[pop_container]
    pop_weights <- pop_weights * (all_upper - all_lower) / (pop_upper[pop_container] - population_bounds[pop_container])
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
