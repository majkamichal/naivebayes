
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Naïve Bayes <img src="man/figures/logo.png" align="right" />

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/naivebayes)](https://cran.r-project.org/package=naivebayes)
[![](http://cranlogs.r-pkg.org/badges/naivebayes)](http://cran.rstudio.com/web/packages/naivebayes/index.html)

## Overview

The `naivebayes` package offers an efficient implementation of the
widely used Naïve Bayes classifier in R. It adheres to three fundamental
principles: `efficiency`, `user-friendliness`, and
`being written in Base R`. The package is designed to be efficient by
utilizing high-performance routines programmed in lower-level languages
like `C` or `FORTRAN` for resource-intensive computations. Despite being
written in `Base R`, it does not introduce any dependencies or hinder
its efficiency. The use of `Base R` ensures compatibility and
accessibility to a wide range of users.

By following these principles, the `naivebayes` package provides a
reliable and efficient tool for Naïve Bayes classification tasks,
ensuring that users can perform their analyses effectively and with
ease.

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
