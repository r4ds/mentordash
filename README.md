
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mentordash

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of mentordash is to create a dashboard for mentors at the [Data
Science Learning Community](https://dslc.io/). Thank you to
[yonicd](https://github.com/yonicd/threads/) for a great deal of work in
the threads package!

The dashboard is now [deployed](http://dslc.io/mentordash)!

## Installation

You can install the released version of mentordash from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Nope.
# install.packages("mentordash")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("r4ds/mentordash")
```

## Example

``` r
library(mentordash)
run_app()
```

## Contributing

Please note that the ‘mentordash’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.

We roughly follow the [tidyverse style
guide](https://style.tidyverse.org/), with the exception that we borrow
the Google convention of prefixing unexported functions with “.”. For
example, while golem defaults to `app_ui` for the main unexported UI
function, we renamed this to `.ui_main`.
