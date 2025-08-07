test_that("Errors work as expected", {
    expect_error(cut_ages("bob"))
    expect_snapshot(error = TRUE, cut_ages("bob"))

    expect_error(cut_ages("bob", 3))
    expect_snapshot(error = TRUE, cut_ages("bob", 3))

    expect_error(cut_ages(3, 3, TRUE))
    expect_snapshot(error = TRUE, cut_ages(3, 3, TRUE))

    expect_error(cut_ages(3, 3, NA_real_))
    expect_snapshot(error = TRUE, cut_ages(3, 3, NA_real_))

    expect_error(cut_ages(3, 3, 1:2))
    expect_snapshot(error = TRUE, cut_ages(3, 3, 1:2))

    expect_error(cut_ages(1:10, breaks = "5L"))
    expect_snapshot(error = TRUE, cut_ages(1:10, breaks = "5L"))

    expect_error(cut_ages(-1:10, 5L))
    expect_snapshot(error = TRUE, cut_ages(-1:10, 5L))

    expect_error(cut_ages(1:10, breaks = NA_integer_))
    expect_snapshot(error = TRUE, cut_ages(1:10, breaks = NA_integer_))

    expect_error(cut_ages(1:10, breaks = c(2L, 2L)))
    expect_snapshot(error = TRUE, cut_ages(1:10, breaks = c(2L, 2L)))

    # multiple limits with ages below minimum errors
    expect_error(cut_ages(c(1:5, 99:102), c(3L, 98L)))
    expect_snapshot(error = TRUE, cut_ages(c(1:5, 99:102), c(3L, 98L)))

    # NA handled correctly
    expect_error(cut_ages(c(NA_integer_, 2:5, 99:102), c(0L, 3L, 98L)))
    expect_snapshot(error = TRUE, cut_ages(c(NA_integer_, 2:5, 99:102), c(0L, 3L, 98L)))

    expect_error(cut_ages(1:10, breaks = c(2L, 9L), max_upper = 7L))
    expect_snapshot(error = TRUE, cut_ages(1:10, breaks = c(2L, 9L), max_upper = 7L))
    expect_error(cut_ages(1:10, breaks = c(9L, 2L)))
    expect_snapshot(error = TRUE, cut_ages(1:10, breaks = c(9L, 2L)))

})

test_that("cut_ages works with single limit works", {
    dat <- 1:10
    limit <- 5L
    lower_bound <- rep.int(c(0, 5), times = c(4, 6))
    upper_bound <- rep.int(c(5, Inf), times = c(4, 6))
    expected <- tibble::tibble(
        interval = factor(
            sprintf("[%s, %s)", lower_bound, upper_bound),
            levels = c("[0, 5)", "[5, Inf)"),
            ordered = TRUE
        ),
        lower = lower_bound,
        upper = upper_bound
    )

    expect_identical(cut_ages(dat, c(0L, limit)), expected)
    expect_identical(tibble::validate_tibble(cut_ages(dat, c(0L, limit))), expected)

})

test_that("cut_ages works with multiple limit works", {
    dat <- c(1:5, 99:102)
    limit <- c(3L, 98L)
    lower_bound <- rep.int(c(0, 3, 98), times = c(2L, 3L, 4L))
    upper_bound <- rep.int(c(3, 98, Inf), times = c(2L, 3L, 4L))
    expected <- tibble::tibble(
        interval = factor(
            sprintf("[%s, %s)", lower_bound, upper_bound),
            levels = c("[0, 3)", "[3, 98)", "[98, Inf)"),
            ordered = TRUE
        ),
        lower = lower_bound,
        upper = upper_bound
    )
    expect_identical(cut_ages(dat, c(0L, limit)), expected)
    expect_identical(tibble::validate_tibble(cut_ages(dat, c(0L, limit))), expected)
})

test_that("cut_ages with limits greater than values works", {
    dat <- 1:5
    limits <- 6:7
    lower_bound <- rep.int(0, 5L)
    upper_bound <- rep.int(6, 5L)
    interval <- sprintf("[%s, %s)", lower_bound, upper_bound)
    expected <- tibble::tibble(
        interval = factor(
            interval,
            levels = c("[0, 6)", "[6, 7)", "[7, Inf)"),
            ordered = TRUE
        ),
        lower = lower_bound,
        upper = upper_bound
    )
    expect_identical(tibble::validate_tibble(cut_ages(dat, c(0L, limits))), expected)
})

test_that("cut_ages works with single age", {
    expected <- tibble::tibble(
        interval = factor("[1, 2)", levels = "[1, 2)", ordered = TRUE),
        lower = 1,
        upper = 2
    )
    expect_identical(tibble::validate_tibble(cut_ages(1, 1, 2)), expected)
})
