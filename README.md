
<!-- README.md is generated from README.Rmd. Please edit that file -->

Extended documentation can be found on the website:
<https://majkamichal.github.io/naivebayes/>

# Naïve Bayes <img src="man/figures/logo.png" align="right" />

[![Build
Status](https://travis-ci.org/majkamichal/naivebayes.svg?branch=master)](https://travis-ci.org/majkamichal/naivebayes)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/naivebayes)](https://cran.r-project.org/package=naivebayes)
[![](http://cranlogs.r-pkg.org/badges/naivebayes)](http://cran.rstudio.com/web/packages/naivebayes/index.html)
[![Say
Thanks:)](https://img.shields.io/badge/Say%20Thanks-!-1EAEDB.svg)](https://saythanks.io/to/majkamichal)

## 1\. Overview

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
them efficient on the dense matrices. In close future sparse matrices
will be supported in order to boost the performance on the sparse data.
Also few helper functions are provided that are supposed to improve the
user experience. The general `naive_bayes()` function is also available
through the excellent `Caret` package.

## 2\. Installation

Just like many other `R` packages, `naivebayes` can be installed from
the `CRAN` repository by simply executing in the console the following
line:

``` r
install.packages("naivebayes")

# Or the the development version from GitHub:
devtools::install_github("majkamichal/naivebayes")
```

## 3\. Usage

The `naivebayes` package provides a user friendly implementation of the
Naïve Bayes algorithm via formula interlace and classical combination of
the matrix/data.frame containing the features and a vector with the
class labels. All functions can recognize missing values, give an
informative warning and more importantly - they know how to handle them.
In following the basic usage of the main function `naive_bayes()` is
demonstrated. Examples with the specialized Naive Bayes classifiers can
be found in the extended documentation:
<https://majkamichal.github.io/naivebayes/>

### 3.1) Example data

``` r
library(naivebayes)

# Simulate example data
n <- 100
set.seed(1)
data <- data.frame(class = sample(c("classA", "classB"), n, TRUE),
                   bern = sample(LETTERS[1:2], n, TRUE),
                   cat  = sample(letters[1:3], n, TRUE),
                   logical = sample(c(TRUE,FALSE), n, TRUE),
                   norm = rnorm(n),
                   count = rpois(n, lambda = c(5,15)))
train <- data[1:95, ]
test <- data[96:100, -1]
```

### 3.2) Formula interface

``` r
nb <- naive_bayes(class ~ ., train)
summary(nb)
#> 
#> ================================ Naive Bayes ================================= 
#>  
#> - Call: naive_bayes.formula(formula = class ~ ., data = train) 
#> - Laplace: 0 
#> - Classes: 2 
#> - Samples: 95 
#> - Features: 5 
#> - Conditional distributions: 
#>     - Bernoulli: 2
#>     - Categorical: 1
#>     - Gaussian: 2
#> - Prior probabilities: 
#>     - classA: 0.5263
#>     - classB: 0.4737
#> 
#> ------------------------------------------------------------------------------

# Classification
predict(nb, test, type = "class")
#> [1] classB classA classA classA classA
#> Levels: classA classB
nb %class% test
#> [1] classB classA classA classA classA
#> Levels: classA classB

# Posterior probabilities
predict(nb, test, type = "prob")
#>         classA    classB
#> [1,] 0.4998488 0.5001512
#> [2,] 0.5934597 0.4065403
#> [3,] 0.6492845 0.3507155
#> [4,] 0.5813621 0.4186379
#> [5,] 0.5087005 0.4912995
nb %prob% test
#>         classA    classB
#> [1,] 0.4998488 0.5001512
#> [2,] 0.5934597 0.4065403
#> [3,] 0.6492845 0.3507155
#> [4,] 0.5813621 0.4186379
#> [5,] 0.5087005 0.4912995

# Helper functions
tables(nb, 1)
#> 
#> ------------------------------------------------------------------------------ 
#>  ::: bern (Bernoulli) 
#> ------------------------------------------------------------------------------ 
#>     
#> bern    classA    classB
#>    A 0.4400000 0.4888889
#>    B 0.5600000 0.5111111
#> 
#> ------------------------------------------------------------------------------
get_cond_dist(nb)
#>          bern           cat       logical          norm         count 
#>   "Bernoulli" "Categorical"   "Bernoulli"    "Gaussian"    "Gaussian"

# Note: all "numeric" (integer, double) variables are modelled
#       with Gaussian distribution by default.
```

### 3.3) Matrix/data.frame and class vector

``` r
X <- train[-1]
class <- train$class
nb2 <- naive_bayes(x = X, y = class)
nb2 %prob% test
#>         classA    classB
#> [1,] 0.4998488 0.5001512
#> [2,] 0.5934597 0.4065403
#> [3,] 0.6492845 0.3507155
#> [4,] 0.5813621 0.4186379
#> [5,] 0.5087005 0.4912995
```

### 3.4) Non-parametric estimation for continuous features

Kernel density estimation can be used to estimate class conditional
densities of continuous features. It has to be explicitly requested via
the parameter `usekernel=TRUE` otherwise Gaussian distribution will be
assumed. The estimation is performed with the built in `R` function
`density()`. By default, Gaussian smoothing kernel and Silverman’s rule
of thumb as bandwidth selector are used:

``` r
nb_kde <- naive_bayes(class ~ ., train, usekernel = TRUE)
summary(nb_kde)
#> 
#> ================================ Naive Bayes ================================= 
#>  
#> - Call: naive_bayes.formula(formula = class ~ ., data = train, usekernel = TRUE) 
#> - Laplace: 0 
#> - Classes: 2 
#> - Samples: 95 
#> - Features: 5 
#> - Conditional distributions: 
#>     - Bernoulli: 2
#>     - Categorical: 1
#>     - KDE: 2
#> - Prior probabilities: 
#>     - classA: 0.5263
#>     - classB: 0.4737
#> 
#> ------------------------------------------------------------------------------
get_cond_dist(nb_kde)
#>          bern           cat       logical          norm         count 
#>   "Bernoulli" "Categorical"   "Bernoulli"         "KDE"         "KDE"
nb_kde %prob% test
#>         classA    classB
#> [1,] 0.6252811 0.3747189
#> [2,] 0.5441986 0.4558014
#> [3,] 0.6515139 0.3484861
#> [4,] 0.6661044 0.3338956
#> [5,] 0.6736159 0.3263841

# Class conditional densities
plot(nb_kde, "norm", arg.num = list(legend.cex = 0.9), prob = "conditional")
```

![](man/figures/kde-1.png)<!-- -->

``` r

# Marginal densities
plot(nb_kde, "norm", arg.num = list(legend.cex = 0.9), prob = "marginal")
```

![](man/figures/kde-2.png)<!-- -->

#### 3.4.1) Changing kernel

In general, there are 7 different smoothing kernels available:

  - `gaussian`
  - `epanechnikov`
  - `rectangular`
  - `triangular`
  - `biweight`
  - `cosine`
  - `optcosine`

and they can be specified in `naive_bayes()` via parameter additional
parameter `kernel`. Gaussian kernel is the default smoothing kernel.
Please see `density()` and `bw.nrd()` for further details.

``` r
# Change Gaussian kernel to biweight kernel
nb_kde_biweight <- naive_bayes(class ~ ., train, usekernel = TRUE,
                               kernel = "biweight")
nb_kde_biweight %prob% test
#>         classA    classB
#> [1,] 0.6237152 0.3762848
#> [2,] 0.5588270 0.4411730
#> [3,] 0.6594737 0.3405263
#> [4,] 0.6650295 0.3349705
#> [5,] 0.6631951 0.3368049
plot(nb_kde_biweight, "norm", arg.num = list(legend.cex = 0.9), prob = "conditional")
```

![](man/figures/kde_kernel-1.png)<!-- -->

#### 3.4.2) Changing bandwidth selector

There are 5 different bandwidth selectors:

  - `nrd0` (Silverman’s rule-of-thumb)
  - `nrd` (variation of the rule-of-thumb)
  - `ucv` (unbiased cross-validation)
  - `bcv` (biased cross-validation)
  - `SJ` (Sheather & Jones method)

They can be specified via `bw` parameter.

``` r
nb_kde_SJ <- naive_bayes(class ~ ., train, usekernel = TRUE,
                               bw = "SJ")
nb_kde_SJ %prob% test
#>         classA    classB
#> [1,] 0.7279209 0.2720791
#> [2,] 0.4858273 0.5141727
#> [3,] 0.7004134 0.2995866
#> [4,] 0.7005704 0.2994296
#> [5,] 0.7089626 0.2910374
plot(nb_kde_SJ, "norm", arg.num = list(legend.cex = 0.9), prob = "conditional")
```

![](man/figures/kde_bw-1.png)<!-- -->

#### 3.4.3) Adjusting bandwidth

Each Bandwidth is chosen according to the bandwidth selector and they
can be additionally adjusted by a factor given by `adjust` parameter:

``` r
nb_kde_adjust <- naive_bayes(class ~ ., train, usekernel = TRUE,
                         adjust = 1.5)
nb_kde_adjust %prob% test
#>         classA    classB
#> [1,] 0.5769725 0.4230275
#> [2,] 0.5953904 0.4046096
#> [3,] 0.6512967 0.3487033
#> [4,] 0.6550197 0.3449803
#> [5,] 0.6024013 0.3975987
plot(nb_kde_adjust, "norm", arg.num = list(legend.cex = 0.9), prob = "conditional")
```

![](man/figures/kde_adjust-1.png)<!-- -->

### 3.5) Model non-negative integers with Poisson distribution

Class conditional distributions of non-negative integer predictors can
be modelled with Poisson distribution. This can be achieved by setting
`usepoisson=TRUE` in the `naive_bayes()` function and by making sure
that the variables representing counts in the dataset are of class
`integer`.

``` r
is.integer(train$count)
#> [1] TRUE
nb_pois <- naive_bayes(class ~ ., train, usepoisson = TRUE)
summary(nb_pois)
#> 
#> ================================ Naive Bayes ================================= 
#>  
#> - Call: naive_bayes.formula(formula = class ~ ., data = train, usepoisson = TRUE) 
#> - Laplace: 0 
#> - Classes: 2 
#> - Samples: 95 
#> - Features: 5 
#> - Conditional distributions: 
#>     - Bernoulli: 2
#>     - Categorical: 1
#>     - Poisson: 1
#>     - Gaussian: 1
#> - Prior probabilities: 
#>     - classA: 0.5263
#>     - classB: 0.4737
#> 
#> ------------------------------------------------------------------------------
get_cond_dist(nb_pois)
#>          bern           cat       logical          norm         count 
#>   "Bernoulli" "Categorical"   "Bernoulli"    "Gaussian"     "Poisson"

nb_pois %prob% test
#>         classA    classB
#> [1,] 0.4815380 0.5184620
#> [2,] 0.4192209 0.5807791
#> [3,] 0.6882270 0.3117730
#> [4,] 0.4794415 0.5205585
#> [5,] 0.5209152 0.4790848

# Class conditional distributions
plot(nb_pois, "count", prob = "conditional")
```

![](man/figures/poisson-1.png)<!-- -->

``` r

# Marginal distributions
plot(nb_pois, "count", prob = "marginal")
```

![](man/figures/poisson-2.png)<!-- -->
