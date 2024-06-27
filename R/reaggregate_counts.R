#' Reaggregate age counts
#'
#' @param bounds `[numeric]`
#'
#' @param counts `[numeric]`
#'
#' @param new_bounds `[numeric]`
#'
#' @param population_bounds  `[numeric]`
#'
#' @param population_weights `[numeric]`
#'
#' @param ... Arguments passed to underlying methods
#'
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
    # lower bounds checks
    if (any(!is.finite(bounds)))
        stop("`bounds` must be a finite, numeric vector.")
    if (!length(bounds))
        stop("`bounds` must be of non-zero length.")
    if (is.unsorted(bounds, na.rm = FALSE, strictly = TRUE))
        stop("`bounds` must be in strictly ascending order")
    if (bounds[1L] < 0)
        stop("`bounds` must be non-negative.")

    # counts checks
    if (any(!is.finite(counts)))
        stop("`counts` must be a finite, numeric vector.")
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
        stop("`new` must be non-negative.")

    # population bounds checks
    if (is.null(population_bounds)) {
        if (!is.null(population_weights))
            stop("`population_weights` require specification of `population_bounds`.")
    } else {
        if (any(!is.finite(population_bounds)))
            stop("`population_bounds` must be a finite, numeric vector.")
        if (!length(population_bounds))
            stop("`population_bounds` must be of non-zero length.")
        if (is.unsorted(population_bounds, na.rm = FALSE, strictly = TRUE))
            stop("`population_bounds` must be in strictly ascending order")
        if (population_bounds[1L] < 0)
            stop("`population_bounds` must be non-negative.")
    }

    # population weight checks#
    if (!is.null(population_weights)) {
        if (any(!is.finite(population_weights)))
            stop("`population_weights` must be a finite, numeric vector.")
        if (length(population_weights) != length(population_bounds))
            stop("`population_weights` must be the same length as `population_bounds`.")
    }

    # Ensure bounds start at zero and adjust rates accordingly
    if (bounds[1L] != 0) {
        bounds <- c(0, bounds)
        counts <- c(0, counts)
    }

    # Ensure new bounds start at zero
    if (new_bounds[1L] != 0)
        new_bounds <- c(0, new_bounds)

    # Ensure population_bounds start at zero and
    if (is.null(population_bounds)) {
        if (new_bounds[length(new_bounds)] > bounds[length(bounds)]) {
            # This approach due to eventual translation
            stop(paste(
                "Where the maximum `new_bound` is greater than the maximum `old_bounds`,",
                "you must specify explicit `population_bounds` and `population_weights`."
            ))
        }
        population_bounds <- new_bounds
    } else if (population_bounds[1L] != 0) {
        population_bounds <- c(0, population_bounds)
    }

    # check that we can actually give an answer
    # TODO - Think we're missing some checks here

    # calculate the combined bounds ignoreing weights for the timebeing
    old_upper <- c(bounds[-1L], Inf)
    new_upper <- c(new_bounds[-1L], Inf)
    pop_upper <- c(population_bounds[-1L], Inf)
    all_lower <- sort(unique(c(bounds, new_bounds)))
    all_upper <- c(all_lower[-1L], Inf)


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

    # we need to scale the weights
    if (is.null(population_weights)) {
        pop_weights <- pop_upper[pop_container] - population_bounds[pop_container]
        pop_weights <- (all_upper - all_lower)
    } else {
        pop_weights <- population_weights[pop_container]
        pop_weights <- pop_weights * (all_upper - all_lower) / (pop_upper[pop_container] - population_bounds[pop_container])
    }

    # now normalise across the new container
    # Note - we can ignore the last one and calculate at the end to avoid handling NaN
    n <- length(new_container)
    pop_weights <- pop_weights[-n]
    weight_dat <- split(pop_weights, old_container[-n])
    weight_dat <- lapply(
        weight_dat,
        function(x) {
            if (sum(x) == 0)
                x <- x + 1L
            x <- x / sum(x)
            x
        }
    )
    weight_dat <- unlist(weight_dat)
    result <- counts[old_container[-n]] * weight_dat
    result[n] <- sum(counts) - sum(result)

    out <- numeric(length(new_bounds))
    idx <- 1L
    for (i in seq_len(n)) {
        if (new_container[i] != idx)
            idx <- idx + 1L
        out[idx] <- out[idx] + result[i]
    }

    interval <- sprintf("[%.f, %.f)", new_bounds, new_upper)
    interval <- factor(interval, levels = interval, ordered = TRUE)

    list2DF(list(
        interval = interval,
        lower = new_bounds,
        upper = new_upper,
        counts = out
    ))

}
