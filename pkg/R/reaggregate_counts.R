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
#' A [tibble][tibble::tbl_df-class] with 4 entries; `interval`, `lower`, `upper`
#' and a corresponding `count`.
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
reaggregate_counts <- function(
    bounds,
    counts,
    new_bounds,
    ...,
    population_bounds = NULL,
    population_weights = NULL
) {

    check_dots_empty0(...)

    # lower bounds checks
    if (!all(is.finite(bounds)))
        stop("`bounds` must be a finite, numeric vector.")

    if (!length(bounds))
        stop("`bounds` must be of non-zero length.")

    if (is.unsorted(bounds, na.rm = FALSE, strictly = TRUE))
        stop("`bounds` must be in strictly ascending order")

    if (bounds[1L] < 0)
        stop("`bounds` must be non-negative.")

    # rates checks
    if (!is.numeric(counts))
        stop("`counts` must be numeric.")

    if (length(counts) != length(bounds))
        stop("`counts` must be the same length as `bounds`.")

    # new bounds checks
    if (!all(is.finite(new_bounds)))
        stop("`new_bounds` must be a finite, numeric vector.")

    if (!length(new_bounds))
        stop("`new_bounds` must be of non-zero length.")

    if (is.unsorted(new_bounds, na.rm = FALSE, strictly = TRUE))
        stop("`new_bounds` must be in strictly ascending order")

    if (new_bounds[1L] < 0)
        stop("`new_bounds` must be non-negative.")

    # population bounds checks
    if (is.null(population_bounds)) {

        if (!is.null(population_weights) && length(population_weights) != length(new_bounds)) {
            stop("When `population_bounds` is not specified, `population_weights` must be the same length as `new_bounds`.") # nolint: line_length_linter.
        }

        if (max(bounds) < max(new_bounds)) {
            stop("Where `population_bounds` are not specified the maximum value of `new_bounds` must be less than or equal to that of `bounds`.") # nolint: line_length_linter.
        }

        population_bounds <- new_bounds

    } else {

        if (!all(is.finite(population_bounds)))
            stop("`population_bounds` must be a finite, numeric vector.")

        if (!length(population_bounds))
            stop("`population_bounds` must be of non-zero length.")

        if (is.unsorted(population_bounds, na.rm = FALSE, strictly = TRUE))
            stop("`population_bounds` must be in strictly ascending order")

        if (population_bounds[1L] < 0)
            stop("`population_bounds} must be non-negative.")

        if (max(bounds) > max(population_bounds)) {
            stop(
                "The maximum value of `bounds` must be less than or equal to that of `population_bounds`." # nolint: line_length_linter.
            )
        }

    }

    # population_weights check
    if (!is.null(population_weights)) {

        if (!all(is.finite(population_weights)) || any(population_weights < 0))
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

    # branch on whether weights or not
    if (is.null(population_weights)) {
        DT <- .reaggregate_counts_unweighted(
            bounds = bounds,
            counts = counts,
            new_bounds = new_bounds
        )
    } else {
        DT <- .reaggregate_counts_weighted(
            bounds = bounds,
            counts = counts,
            new_bounds = new_bounds,
            population_bounds = population_bounds,
            population_weights = population_weights
        )
    }

    # calculate the new upper bounds
    new_upper <- c(new_bounds[-1L], Inf)

    # calculate the interval
    interval <- sprintf("[%.f, %.f)", new_bounds, new_upper)
    interval <- factor(interval, levels = interval, ordered = TRUE)

    # return as tibble
    new_tibble(
        list(
            interval = interval,
            lower = new_bounds,
            upper = new_upper,
            count = DT$count
        )
    )
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.reaggregate_counts_unweighted <- function(bounds, counts, new_bounds) {
    all_lower <- sort(unique(c(bounds, new_bounds)))

    # vctrs::new_data_frame should be safe to use here due to earlier input
    #   checks in the user facing function
    dat <- new_data_frame(list(lower = bounds, counts = counts))
    cut <- cut_ages(all_lower, breaks = bounds)
    dat <- merge(cut, dat, by = "lower")
    out <- cut_ages(all_lower, breaks = new_bounds)
    fraction <- (c(all_lower[-1L], NA) - all_lower) / (dat$upper - dat$lower)
    fraction[is.na(fraction)] <- 1
    out$count <- fraction * dat$counts
    # The following is optimised for performance for our use cases but is the
    # equivalent (save output type) of
    # setDT(out)[, .(count = sum(count)), by = "lower"][]
    .fast_grouped_sum(out$count, out$lower, byname = "lower", sumname = "count")
}

# -------------------------------------------------------------------------

.reaggregate_counts_weighted <- function(
    bounds,
    counts,
    new_bounds,
    population_bounds,
    population_weights
) {
    all_lower <- sort(unique(c(bounds, new_bounds, population_bounds)))
    # vctrs::new_data_frame should be safe to use here due to earlier input
    #   checks in the user facing function
    dat <- new_data_frame(list(lower = bounds, counts = counts))
    dat10 <- cut_ages(all_lower, breaks = bounds)
    dat0 <- merge(dat10, dat, by = "lower")

    dat1 <- .reaggregate_counts_unweighted(population_bounds, population_weights, all_lower)
    names(dat1) <- c("lower", "w")
    dat3 <- cut_ages(all_lower, breaks = new_bounds)

    out <- dat0
    out$lower <- all_lower
    out <- merge(out, dat1, by = "lower")
    out$i <- dat0$lower
    # The following is optimised for performance for our use cases but is the
    # equivalent (save output type) of
    # out <- out[, .(ck = counts * w/sum(w)), by = "i"]
    # set(out, j = "lower", value = dat3$lower)
    # out[, .(count = sum(ck)), keyby = "lower"][]
    out$ck <- out$counts * out$w / .ave_sum(out$w, out$i)
    .fast_grouped_sum(out$ck, dat3$lower, byname = "lower", sumname = "count")
}
