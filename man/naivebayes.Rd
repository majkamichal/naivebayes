\docType{package}
\name{naivebayes}
\alias{naivebayes}
\alias{naivebayes-package}
\title{naivebayes}
\description{

The naivebayes package provides an efficient implementation of the popular Naive Bayes classifier in R. It was developed and is now maintained based on three principles: it should be efficient, user friendly and written in Base R. The last implies no dependencies, however, it neither denies nor interferes with being efficient as many functions from the Base R distribution use highly efficient routines programmed in lower level languages, such as C or FORTRAN. In fact, the naivebayes package utilizes only such functions for resource-intensive calculations.
}
\details{

The general function \bold{\code{naive_bayes()}} detects the class of each feature in the dataset and, depending on the user choices, assumes possibly different distribution for each feature. It currently supports following class conditional distributions:

\itemize{
    \item categorical distribution for discrete features
    \item Poisson distribution for non-negative integers
    \item Gaussian distribution for continuous features
    \item non-parametrically estimated densities via Kernel Density Estimation for continuous features
}

In addition to that specialized functions are available which implement:

\itemize{
    \item Bernoulli Naive Bayes via \bold{\code{bernoulli_naive_bayes()}}
    \item Multinomial Naive Bayes via \bold{\code{multinomial_naive_bayes()}}
    \item Poisson Naive Bayes via \bold{\code{poisson_naive_bayes()}}
    \item Gaussian Naive Bayes via \bold{\code{gaussian_naive_bayes()}}
    \item Non-Parametric Naive Bayes via \bold{\code{nonparametric_naive_bayes()}}
}

\bold{Extended documentation can be found on the website:}
\itemize{
    \item \url{https://majkamichal.github.io/naivebayes/}
}

\bold{Bug reports:}
\itemize{
    \item \url{https://github.com/majkamichal/naivebayes/issues}
}

\bold{Suggestions:}
\itemize{
    \item \email{michalmajka@hotmail.com}
}

}
