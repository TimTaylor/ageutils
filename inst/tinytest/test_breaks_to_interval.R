brks <- c(-55.3, 0, 10.3, 1000)
lower <- c(-55,0,10,1000)
upper <- c(0,10,1000,Inf)
interval <- sprintf("[%.f, %.f)", lower, upper)
interval <- factor(interval, levels = interval, ordered = TRUE)
expected <- data.frame(interval, lower_bound=lower, upper_bound=upper)
expect_equal(breaks_to_interval(brks), expected)
expect_identical(breaks_to_interval(brks), ageutils:::breaks_to_interval_r(brks))

brks[1L] <- Inf
expect_error(suppressWarnings(breaks_to_interval(brks)))

brks[1L] <- NA_real_
expect_error(breaks_to_interval(brks))

brks[1L] <- 1
expect_error(breaks_to_interval(brks))

brks[1L] <- 0
expect_error(breaks_to_interval(brks))

brks[1L] <- -3
expect_silent(breaks_to_interval(brks))

expect_error(breaks_to_interval(TRUE))

expect_error(suppressWarnings(breaks_to_interval(.Machine$integer.max + 1)))

expect_error(breaks_to_interval(1, TRUE))

expect_error(breaks_to_interval(1, 1:2))

expect_error(breaks_to_interval(1, NA_real_))

expect_error(breaks_to_interval(1, 1))

# single breaks
expected <- data.frame(
    interval = factor("[1, 2)", levels = "[1, 2)", ordered = TRUE),
    lower_bound = 1,
    upper_bound = 2
)
expect_identical(breaks_to_interval(1,2), expected)
