
<!-- README.md is generated from README.Rmd. Please edit that file -->
naivebayes
==========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/naivebayes)](https://cran.r-project.org/package=gnaivebayes) [![](http://cranlogs.r-pkg.org/badges/naivebayes)](http://cran.rstudio.com/web/packages/naivebayes/index.html)

Overview
--------

naivebayes is an efficient implementation of the Naive Bayes algorithm in R.

Installation
------------

``` r
install.packages("naivebayes")

# Or the the development version from GitHub:
devtools::install_github("majkamichal/naivebayes")
```

Usage
-----

``` r
library(naivebayes)

data(iris)
nb <- naive_bayes(Species ~ ., data = iris)

tables(nb, which = "Sepal.Length")
#> $Sepal.Length
#>             
#> Sepal.Length    setosa versicolor virginica
#>         mean 5.0060000  5.9360000 6.5880000
#>         sd   0.3524897  0.5161711 0.6358796

plot(nb, which = "Sepal.Length")
```

![](README-example-1.png)

``` r

head(predict(nb))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
```
