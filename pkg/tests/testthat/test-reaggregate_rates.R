test_that("reaggregate_rates without weights works", {
    bounds <- c(0,  10,  50, 100, 200)
    rates <- as.double(seq_along(bounds))
    out <- reaggregate_rates(bounds = bounds, rates = rates, new_bounds = bounds)
    expected <- tibble::tibble(lower = bounds, upper = c(bounds[-1], Inf), rate = rates)
    expect_identical(out[-1L], expected)

    bounds2 <- c(0, 5, 10, 25, 50, 75, 100, 150, 200)
    out <- reaggregate_rates(bounds = bounds, rates = rates, new_bounds = bounds2)
    expected <- tibble::tibble(
        lower = bounds2,
        upper = c(bounds2[-1], Inf),
        rate = head(rep(rates, each = 2), -1)
    )
    expect_identical(out[-1L], expected)

    out <- reaggregate_rates(bounds = bounds, rates = rates, new_bounds = 200)
    expected <- tibble::tibble(lower = c(0, 200), upper = c(200, Inf), rate = c(3.2, 5))
    expect_identical(out[-1L], expected)

    bounds <- c(0, 1, 5)
    rates <- c(0.15, 0.30, 0.80)
    dat <- reaggregate_rates(bounds, rates, new_bounds = c(0, 5))
    expect_equal(
        dat$rate[1],
        ((bounds[3] - bounds[2]) * rates[2] + (bounds[2] - bounds[1]) * rates[1]) / (bounds[3] - bounds[1]) # nolint: line_length_linter.
    )
})

test_that("reaggregate_rates with weights works - example 1", {

    skip_if_not_installed("dplyr")

    bounds <- c(0, 10, 50, 100, 180)
    rates <- c(0.1, 0.3, 0.2, 0.1, 0.5)
    new_bounds <- c(0, 60, 150, 180)
    population_bounds <- c(0, 80, 150, 180)
    population_weights <- c(10, 20, 30, 40)

    current <- reaggregate_rates(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    target <- reaggregate_rates_edwin_weighted(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    current2 <- current[-c(1, 3)]
    expect_equal(
        current2,
        setNames(target, names(current2))
    )

    target2 <- reaggregate_rates_fast(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    expect_equal(
        current,
        target2
    )

})

test_that("reaggregate_rates with weights works - example 2", {

    skip_if_not_installed("dplyr")

    bounds <- c(0, 10, 50, 100, 160)
    rates <- c(0.1, 0.3, 0.2, 0.1, 0.5)
    new_bounds <- c(0, 60, 80, 150, 170, 180)
    population_bounds <- c(0, 80, 150, 180)
    population_weights <- c(10, 20, 30, 40)

    current <- reaggregate_rates(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    target <- reaggregate_rates_edwin_weighted(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    current2 <- current[-c(1, 3)]
    expect_equal(
        current2,
        setNames(target, names(current2))
    )

    target2 <- reaggregate_rates_fast(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    expect_equal(
        current,
        target2
    )

})

test_that("reaggregate_rates with weights works - example 3", {
    skip_if_not_installed("dplyr")

    bounds <- 0
    rates <- 532.6622
    new_bounds <- c(0, 5, 15, 45, 65)
    population_weights <- c(
        601913, 625476, 650226, 671016, 690816, 689190, 694734, 709940,
        730548, 715046, 703087, 692873, 698821, 676773, 664025, 637756,
        628023, 613025, 606611, 630456, 653155, 679487, 694216, 720400,
        725264, 725111, 745909, 747190, 768513, 784770, 771964, 764738,
        773176, 753953, 760821, 758955, 741034, 745909, 743814, 748497,
        749883, 720148, 672256, 661208, 672808, 685301, 696231, 724675,
        754435, 777997, 758905, 775715, 774083, 784116, 782532, 784299,
        775320, 756023, 736939, 709201, 677962, 660419, 644896, 618893,
        594643, 571143, 569908, 559844, 541297, 542108, 550059, 560762,
        587908, 632852, 482547, 461855, 453442, 413706, 362412, 318577,
        323778, 313081, 294838, 271063, 246429, 223058, 195010, 173861,
        156497, 136917, 521067
    )
    population_bounds <- seq_along(population_weights) - 1L

    current <- reaggregate_rates(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    target <- reaggregate_rates_edwin_weighted(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    current2 <- current[-c(1, 3)]
    expect_equal(
        current2,
        setNames(target, names(current2))
    )

    target2 <- reaggregate_rates_fast(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    expect_equal(
        current,
        target2
    )
})

test_that("reaggregate_rates with weights works - example 4", {

    skip_if_not_installed("dplyr")

    bounds <- 0:99
    rates <- rep(seq(25, 5, -5), each = 20)
    new_bounds <- c(0, 5, 15, 45, 65)
    population_bounds <- 0:99
    population_weights <- 1:100

    current <- reaggregate_rates(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    target <- reaggregate_rates_edwin_weighted(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    current2 <- current[-c(1, 3)]
    expect_equal(
        current2,
        setNames(target, names(current2))
    )

    target2 <- reaggregate_rates_fast(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    expect_equal(
        current,
        target2
    )

})

test_that("reaggregate_rates matches Edwins for Neil's bug report", {

    skip_if_not_installed("dplyr")

    reaggregate_rates(bounds = 0:99,
                      rates = rep(seq(25, 5, -5), each = 20),
                      new_bounds = c(0, 5, 15, 45, 65),
                      population_bounds = 0:99,
                      population_weights = 1:100)

    bounds <- 0:99
    rates = rep(seq(25, 5, -5), each = 20)
    new_bounds = c(0, 5, 15, 45, 65)
    population_bounds <- bounds
    population_weights <- bounds + 1

    current <- reaggregate_rates(
        bounds = bounds,
        rates = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    target <- reaggregate_rates_edwin_weighted(
        bounds = bounds,
        rates = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    current2 <- current[-c(1, 3)]
    expect_equal(
        current2,
        setNames(target, names(current2))
    )

    target2 <- reaggregate_rates_fast(
        bounds = bounds,
        rates  = rates,
        new_bounds = new_bounds,
        population_bounds = population_bounds,
        population_weights = population_weights
    )

    expect_equal(
        current,
        target2
    )
})
