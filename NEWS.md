# ageutils (development version)

* `breaks_to_interval()` and `cut_ages()` both gain an argument, `max_upper`
  which allows users to explicitly set the maximum upper bound.
  
* New function `reaggregate_interval_rates()`.

* Restructuring of documentation and internal workings.

# ageutils 0.0.1

Initial release of `ageutils` which provides a collection of efficient functions
for working with individual ages and corresponding intervals. These include
functions for efficient conversion from an age to an interval, aggregation of
ages with associated counts in to intervals and the splitting of interval counts
based on specified age distributions.

Functions are derived from those in the
[ympes](https://cran.r-project.org/package=ympes) with the intention being to
remove these functions from that package in favour of this one going forward.