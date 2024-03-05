# single limit
dat <- 1:10
limit <- 5L
lower_bound <- rep.int(c(0, 5), times = c(4, 6))
upper_bound <- rep.int(c(5, Inf), times = c(4, 6))
expected <- data.frame(
    interval = factor(
        sprintf("[%s, %s)", lower_bound, upper_bound),
        levels = c("[0, 5)", "[5, Inf)"),
        ordered = TRUE
    ),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)

expect_identical(cut_ages(dat, c(0L, limit)), expected)
expect_identical(cut_ages(dat, c(0L, limit)), ageutils:::cut_ages_r(dat, c(0L, limit)))

# multiple limits
dat <- c(1:5, 99:102)
limit <- c(3L, 98L)
lower_bound <- rep.int(c(0, 3, 98), times = c(2L, 3L, 4L))
upper_bound <- rep.int(c(3, 98, Inf), times = c(2L, 3L, 4L))
expected <- data.frame(
    interval = factor(
        sprintf("[%s, %s)", lower_bound, upper_bound),
        levels = c("[0, 3)", "[3, 98)", "[98, Inf)"),
        ordered = TRUE
    ),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)
expect_identical(cut_ages(dat, c(0L, limit)), expected)
expect_identical(cut_ages(dat, c(0L, limit)), ageutils:::cut_ages_r(dat, c(0L, limit)))

# multiple limits with ages below minimum errors
dat <- c(1:5, 99:102)
limit <- c(3L, 98L)
lower_bound <- rep.int(c(NA_real_, 3, 98), times = c(2L, 3L, 4L))
upper_bound <- rep.int(c(NA_real_, 98, Inf), times = c(2L, 3L, 4L))
expect_error(
    cut_ages(dat, limit),
    "`ages` must greater than or equal to the minimum value of `breaks`.",
    fixed = TRUE
)


# NA handled correctly
dat <- c(1:5, 99:102)
dat[[1L]] <- NA_integer_
limit <- c(3L, 98L)
expect_error(
    cut_ages(dat, c(0L, limit)),
    "`ages` must be non-missing (not NA) and coercible to integer.",
    fixed = TRUE
)


# limits greater than values
dat <- 1:5
limits <- 6:7
lower_bound <- rep.int(0, 5L)
upper_bound <- rep.int(6, 5L)
interval <- sprintf("[%s, %s)", lower_bound, upper_bound)
expected <- data.frame(
    interval = factor(
        interval,
        levels = c("[0, 6)", "[6, 7)", "[7, Inf)"),
        ordered = TRUE
    ),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)
expect_identical(cut_ages(dat, c(0L, limits)), expected)
expect_identical(cut_ages(dat, c(0L, limits)), ageutils:::cut_ages_r(dat, c(0L, limits)))

# single age
expected <- data.frame(
    interval = factor("[1, 2)", levels = "[1, 2)", ordered = TRUE),
    lower_bound = 1,
    upper_bound = 2
)
expect_identical(cut_ages(1,1,2), expected)
expect_identical(cut_ages(1,1,2), ageutils:::cut_ages_r(1,1,2))

# error messaging
expect_error( cut_ages("bob") )
expect_error( cut_ages("bob", 3) )
expect_error( cut_ages(3, 3, TRUE) )
expect_error( cut_ages(3, 3, NA_real_) )
expect_error( cut_ages(3, 3, 1:2) )
expect_error( cut_ages(1:10, breaks = "5L") )
expect_error( cut_ages(-1:10, 5L) )

expect_error(
    cut_ages(1:10, breaks = NA_integer_),
    "`breaks` must be non-negative and coercible to integer.",
    fixed = TRUE
)

expect_error(
    cut_ages(1:10, breaks = c(2L, 2L)),
    "`breaks` must be in strictly increasing order.",
    fixed = TRUE
)
