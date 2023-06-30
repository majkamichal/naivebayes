
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Naïve Bayes <img src="docs/reference/figures/logo.png" align="right" />

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/naivebayes)](https://cran.r-project.org/package=naivebayes)
[![](http://cranlogs.r-pkg.org/badges/naivebayes)](http://cran.rstudio.com/web/packages/naivebayes/index.html)

## Overview

The `naivebayes` package provides an efficient implementation of the
popular Naïve Bayes classifier in `R`. It was developed and is now
maintained based on three principles: it should be efficient, user
friendly and written in `Base R`. The last implies no dependencies,
however, it neither denies nor interferes with being efficient as many
functions from the `Base R` distribution use highly efficient routines
programmed in lower level languages, such as `C` or `FORTRAN`. In fact,
the `naivebayes` package utilizes only such functions for
resource-intensive calculations.

The general function `naive_bayes()` detects the class of each feature
in the dataset and, depending on the user choices, assumes possibly
different distribution for each feature. It currently supports following
class conditional distributions:

- categorical distribution for discrete features
- Poisson distribution for non-negative integers
- Gaussian distribution for continuous features
- non-parametrically estimated densities via Kernel Density Estimation
  for continuous features

In addition to that specialized functions are available which implement:

- Bernoulli Naive Bayes via `bernoulli_naive_bayes()`
- Multinomial Naive Bayes via `multinomial_naive_bayes()`
- Poisson Naive Bayes via `poisson_naive_bayes()`
- Gaussian Naive Bayes via `gaussian_naive_bayes()`
- Non-Parametric Naive Bayes via `nonparametric_naive_bayes()`

They are implemented based on the linear algebra operations which makes
them efficient on the dense matrices. They can also take advantage of
**sparse matrices** to furthermore boost the performance. Also few
helper functions are provided that are supposed to improve the user
experience. The general `naive_bayes()` function is also available
through the excellent `Caret` package.

## Installation

Just like many other `R` packages, `naivebayes` can be installed from
the `CRAN` repository by simply executing in the console the following
line:

``` r
install.packages("naivebayes")

# Or the the development version from GitHub:
devtools::install_github("majkamichal/naivebayes")
```
