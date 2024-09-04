bounds <- c(0,  10,  50, 100, 200)
rates <- as.double(seq_along(bounds))
out <- reaggregate_rates(bounds = bounds, rates = rates, new_bounds = bounds)
expected <- tibble::tibble(lower = bounds, upper = c(bounds[-1], Inf), rate = rates)
expect_identical(out[-1L], expected)

bounds2 <- c(0, 5, 10, 25, 50, 75, 100, 150, 200, 250)
out <- reaggregate_rates(bounds = bounds, rates = rates, new_bounds = bounds2)
expected <- tibble::tibble(lower = bounds2, upper = c(bounds2[-1], Inf), rate = rep(rates, each = 2))
expect_identical(out[-1L], expected)

out <- reaggregate_rates(bounds = bounds, rates = rates, new_bounds = 200)
expected <- tibble::tibble(lower = c(0,200), upper = c(200, Inf), rate = c(3.2, 5))
expect_identical(out[-1L], expected)


bounds <- c(0, 1, 5)
rates <- c(0.15, 0.30, 0.80)
dat <- reaggregate_rates(bounds, rates, new_bounds = c(0, 5, 10))
expect_equal(
    dat$rate[1],
    (bounds[3] - bounds[2]) * rates[2] + (bounds[2] - bounds[1]) * rates[1]) / (bounds[3] - bounds[1])
)


# reaggregate_rates(
#     bounds = c(  0,  10,  50, 100, 200),
#     rates  = c(0.1, 0.3, 0.2, 0.1, 0.5),
#     new_bounds = c(0, 60, 150, 180, 205),
#     population_bounds = c(0,80,150,180),
#     population_weights = c(10,20,30,40)
# )

