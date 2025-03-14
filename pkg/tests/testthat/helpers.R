reaggregate_counts_edwin_unweighted <- function(bounds, counts, new_bounds) {
    # bounds <- c(0, 80, 150, 180)
    # counts <- c(10, 20, 30, 40)
    # new_bounds <- c(0, 60, 150, 160, 180)
    # stop()
    # As far as I can tell we need this test! Note this is not in the current reaggregate_rates?
    stopifnot(max(bounds) >= max(new_bounds))
    tibble::tibble(lower_bound = bounds, counts = counts) -> dat

    all_lower <- sort(unique(c(bounds, new_bounds)))
    cut_ages(all_lower, breaks = bounds) |>
        dplyr::left_join(dat, by = dplyr::join_by(lower_bound)) |>
        dplyr::mutate(all_lower = all_lower, fraction = (dplyr::lead(all_lower) - all_lower) / (upper_bound - lower_bound)) |>
        # Last age group
        dplyr::mutate(fraction = ifelse(is.na(fraction), 1, fraction)) |>
        dplyr::mutate(count = fraction * counts) -> dat

    dat |>
        dplyr::select(all_lower, count) -> dat0

    cut_ages(all_lower, breaks = new_bounds) |>
        dplyr::mutate(count = dat0$count) |>
        dplyr::summarise(count = sum(count), .by = c(lower_bound))
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
            tibble::tibble(lower_bound = bounds, count = counts),
            by = dplyr::join_by(lower_bound)) -> dat0

    dat1 <- reaggregate_counts_edwin_unweighted(population_bounds, population_weights, all_lower)
    # c_k = c_i N_k/N_i, where i is old bounds, k is new bounds
    cut_ages(all_lower, breaks = new_bounds) -> dat3
    dat0 |>
        dplyr::mutate(lower_bound = all_lower) |>
        dplyr::left_join(dplyr::rename(dat1, w = count), by = dplyr::join_by(lower_bound)) |>
        dplyr::mutate(i = dat0$lower_bound) |>
        dplyr::mutate(ck = count * w/sum(w), .by = c(i)) |>
        dplyr::mutate(lower_bound = dat3$lower_bound) |>
        dplyr::summarise(count = sum(ck), .by = c(lower_bound)) -> dat5

    stopifnot(sum(dat5$count) == sum(counts))
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
    tibble::tibble(lower_bound = bounds, rates = rates) -> dat
    all_lower <- sort(unique(c(bounds, new_bounds, population_bounds)))
    cut_ages(all_lower, breaks = bounds) |>
        dplyr::left_join(dat, by = dplyr::join_by(lower_bound)) -> dat1
    dat2 <- reaggregate_counts_edwin_unweighted(population_bounds, population_weights, all_lower)
    dat1 |>
        dplyr::mutate(weight = dat2$count) -> dat3
    cut_ages(all_lower, breaks = new_bounds) -> dat4
    dat3 |>
        dplyr::mutate(lower_bound = dat4$lower_bound) |>
        dplyr::summarise(rate = sum(rates * weight) / sum(weight), .by = c(lower_bound))
}
