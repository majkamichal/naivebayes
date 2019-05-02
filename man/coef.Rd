\name{coef}
\alias{coef.bernoulli_naive_bayes}
\alias{coef.multinomial_naive_bayes}
\alias{coef.poisson_naive_bayes}
\alias{coef.gaussian_naive_bayes}

\title{Extract Model Coefficients}

\usage{
\method{coef}{bernoulli_naive_bayes}(object, ...)

\method{coef}{multinomial_naive_bayes}(object, ...)

\method{coef}{poisson_naive_bayes}(object, ...)

\method{coef}{gaussian_naive_bayes}(object, ...)
}

\arguments{
\item{object}{object of class inheriting from \code{"*_naive_bayes"}.}
\item{...}{not used.}
}

\value{
Coefficients extracted from the specialised Naive Bayes objects in form of a data frame.
}

\description{
\code{coef} is a generic function which extracts model coefficients from specialized Naive Bayes objects.
}

\examples{
data(iris)
y <- iris[[5]]
M <- as.matrix(iris[-5])

### Train the Gaussian Naive Bayes
gnb <- gaussian_naive_bayes(x = M, y = y)

### Extract coefficients
coef(gnb)

coef(gnb)[c(TRUE,FALSE)] # only means

coef(gnb)[c(FALSE,TRUE)] # only standard deviations
}

\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{bernoulli_naive_bayes}}, \code{\link{multinomial_naive_bayes}}, \code{\link{poisson_naive_bayes}}, \code{\link{gaussian_naive_bayes}}
}
