\docType{package}
\name{naivebayes}
\alias{naivebayes}
\alias{naivebayes-package}
\title{naivebayes}

\description{

The \bold{naivebayes} package presents an efficient implementation of the widely-used Naive Bayes classifier. It upholds three core principles: efficiency, user-friendliness, and reliance solely on Base R. By adhering to the latter principle, the package ensures stability and reliability without introducing external dependencies. This design choice maintains efficiency by leveraging the optimized routines inherent in Base R, many of which are programmed in high-performance languages like C/C++ or FORTRAN. By following these principles, the naivebayes package provides a reliable and efficient tool for Naive Bayes classification tasks, ensuring that users can perform their analyses effectively and with ease, even in the presence of missing data.

}

\details{

The general \bold{\code{naive_bayes()}} function is designed to determine the class of each feature in a dataset, and depending on user specifications, it can assume various distributions for each feature. It currently supports the following class conditional distributions:

\itemize{
  \item Categorical distribution for discrete features (with Bernoulli distribution as a special case for binary outcomes)
  \item Poisson distribution for non-negative integer features
  \item Gaussian distribution for continuous features
  \item non-parametrically estimated densities via Kernel Density Estimation for continuous features
}

In addition to the general Naive Bayes function, the package provides specialized functions for various types of Naive Bayes classifiers. The specialized functions are carefully optimized for efficiency, utilizing linear algebra operations to excel when handling dense matrices. Additionally, they can also exploit sparsity of matrices for enhanced performance:

\itemize{
 \item Bernoulli Naiive Bayes via \bold{\code{bernoulli_naive_bayes()}}
 \item Multinomial Naive Bayes via \bold{\code{multinomial_naive_bayes()}}
 \item Poisson Naive Bayes via \bold{\code{poisson_naive_bayes()}}
 \item Gaussian Naive Bayes via \bold{\code{gaussian_naive_bayes()}}
 \item Non-Parametric Naive Bayes via \bold{\code{nonparametric_naive_bayes()}}
}

These specialized classifiers are tailored to different assumptions about the underlying data distributions, offering users versatile tools for classification tasks. Moreover, the package incorporates various helper functions aimed at enhancing the user experience. Notably, the model fitting functions provided by the package can effectively handle missing data, ensuring that users can utilize the classifiers even in the presence of incomplete information.

\bold{Extended documentation can be found on the website:}
\itemize{
    \item \url{https://majkamichal.github.io/naivebayes/}
}

\bold{Bug reports:}
\itemize{
    \item \url{https://github.com/majkamichal/naivebayes/issues}
}

\bold{Contact:}
\itemize{
    \item \email{michalmajka@hotmail.com}
}

}
