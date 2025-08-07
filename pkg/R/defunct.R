# -------------------------------------------------------------------------
#' Defunct functions
#'
# -------------------------------------------------------------------------
#' @description
#' Functions removed from ageutils in the 0.0.8 release. They have been replaced
#' with stubs that will error and report them as defunct.
#'
#' - `reaggregate_interval_rates()` (best replaced by `reaggregate_rates()`),
#' - `reaggregate_interval_counts()` (best replaced by `reaggregate_counts()`),
#' - `split_interval_counts()`, and
#' - `aggregate_age_counts()`.
#'
# -------------------------------------------------------------------------
#' @name ageutils-defunct
#' @keywords internal
NULL



# -------------------------------------------------------------------------
#' @rdname ageutils-defunct
#' @export
reaggregate_interval_rates <- function(
    lower_bounds,
    upper_bounds = NULL,
    rates,
    breaks,
    weights = NULL
) {

    .Defunct(
        msg = paste(
            "`reaggregate_interval_rates` was removed in the 0.0.8 release of ageutils.",
            "Going forward please use the function `reaggregate_rates()`."
        ),
        new = "reaggregate_rates"
    )
}

# -------------------------------------------------------------------------
#' @rdname ageutils-defunct
#' @export
reaggregate_interval_counts <- function(
    lower_bounds,
    upper_bounds,
    counts,
    breaks,
    max_upper = 100L,
    weights = NULL
) {
    .Defunct(
        msg = paste(
            "`reaggregate_interval_counts` was removed in the 0.0.8 release of ageutils.",
            "Going forward please use the function `reaggregate_counts()`."
        ),
        new = "reaggregate_counts"
    )
}

# -------------------------------------------------------------------------
#' @rdname ageutils-defunct
#' @export
split_interval_counts <- function(
    lower_bounds,
    upper_bounds,
    counts,
    max_upper = 100L,
    weights = NULL
) {
    .Defunct(
        msg = "`split_interval_counts` was removed in the 0.0.8 release of ageutils."
    )
}

# -------------------------------------------------------------------------
#' @rdname ageutils-defunct
#' @export
aggregate_age_counts <- function(
    counts,
    ages = seq_along(counts) - 1L,
    breaks
) {
    .Defunct(
        msg = "`aggregate_age_counts` was removed in the 0.0.8 release of ageutils."
    )
}
