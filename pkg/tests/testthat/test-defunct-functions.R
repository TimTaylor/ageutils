test_that("defunct functions are defunct!", {
    expect_error(reaggregate_interval_rates())
    expect_error(reaggregate_interval_counts())
    expect_error(split_interval_counts())
    expect_error(aggregate_age_counts())
})

test_that("defunct functions give good error messaging!", {
    expect_snapshot(error = TRUE, cnd_class = TRUE, reaggregate_interval_rates())
    expect_snapshot(error = TRUE, cnd_class = TRUE, reaggregate_interval_counts())
    expect_snapshot(error = TRUE, cnd_class = TRUE, split_interval_counts())
    expect_snapshot(error = TRUE, cnd_class = TRUE, aggregate_age_counts())
})
