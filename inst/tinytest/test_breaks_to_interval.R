brks <- c(-55.3, 0, 10.3, 1000)
lower <- c(-55,0,10,1000)
upper <- c(0,10,1000,Inf)
interval <- sprintf("[%.f, %.f)", lower, upper)
interval <- factor(interval, levels = interval, ordered = TRUE)
expected <- data.frame(interval, lower_bound=lower, upper_bound=upper)
expect_equal(breaks_to_interval(brks), expected)

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
