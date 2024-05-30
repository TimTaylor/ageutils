# ageutils 0.0.3

* Internal changes only.

# ageutils 0.0.2

*  `aggregate_age_counts()` will now only return a row corresponding to NA ages
  if they were present in the input data. Previously an NA-associated row would
  always be returned even if it's count was 0. Due to this change
  `reaggregate_interval_counts()` will now never return an NA-associated row.

* `split_interval_counts()` now matches the documentation and disallows missing
  (NA) bounds.

* `breaks_to_interval()` and `cut_ages()` both gain an argument, `max_upper`
  which allows users to explicitly set the maximum upper bound.
  
* New function `reaggregate_interval_rates()`.

* For the vignette we now use
  [markdown](https://cran.r-project.org/package=markdown) as a lighter
  alternative to [rmarkdown](https://cran.r-project.org/package=rmarkdown).

# ageutils 0.0.1

Initial release of `ageutils` which provides a collection of efficient functions
for working with individual ages and corresponding intervals. These include
functions for efficient conversion from an age to an interval, aggregation of
ages with associated counts in to intervals and the splitting of interval counts
based on specified age distributions.

Functions are derived from those in the
[ympes](https://cran.r-project.org/package=ympes) with the intention being to
remove these functions from that package in favour of this one going forward.
