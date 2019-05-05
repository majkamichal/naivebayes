\name{gaussian_naive_bayes}
\alias{gaussian_naive_bayes}

\title{Gaussian Naive Bayes Classifier}
\usage{
gaussian_naive_bayes(x, y, prior = NULL, ...)
}
\arguments{
\item{x}{matrix with metric predictors (only numeric matrix accepted).}

\item{y}{class vector (character/factor/logical).}

\item{prior}{vector with prior probabilities of the classes. If unspecified, the class proportions for the training set are used. If present, the probabilities should be specified in the order of the factor levels.}

\item{...}{not used.}

}

\value{
\code{gaussian_naive_bayes} returns an object of class \code{"gaussian_naive_bayes"} which is a list with following components:
    \item{data}{list with two components: \code{x} (matrix with predictors) and \code{y} (class variable).}
    \item{levels}{character vector with values of the class variable.}
    \item{params}{list with two matrices, first containing the class conditional means and the second containing the class conditional standard deviations.}
    \item{prior}{numeric vector with prior probabilities.}
    \item{call}{the call that produced this object.}
}
\description{
\code{gaussian_naive_bayes} is used to fit the Gaussian Naive Bayes model in which all class conditional distributions are assumed to be Gaussian and be independent.
}

\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on real values (numeric/integer) and class conditional probabilities are modelled with the Gaussian distribution.

The Gaussian Naive Bayes is available in both, \code{naive_bayes} and \code{gaussian_naive_bayes}. The implementation of the specialized Naive Bayes provides more efficient performance though. The speedup comes from the restricting the data input to a numeric matrix and performing the linear algebra as well vectorized operations on it. In other words, the efficiency comes at cost of the flexibility.

The \code{gaussian_naive_bayes} function is equivalent to the \code{naive_bayes} function when the numeric matrix or a data.frame contains only numeric variables.

The missing values (NAs) are omitted during the parameter estimation. The NAs in the newdata in \code{predict.gaussian_naive_bayes} are not included into the calculation of posterior probabilities; and if present an informative warning is given.
}


\examples{
data(iris)
y <- iris[[5]]
M <- as.matrix(iris[-5])

### Train the Gaussian Naive Bayes
gnb <- gaussian_naive_bayes(x = M, y = y)
summary(gnb)
head(predict(gnb, newdata = M, type = "prob"))

###  Equivalent calculation with general naive_bayes function.
nb <- naive_bayes(M, y)
head(predict(nb, type = "prob"))

# Obtain probability tables
tables(gnb, which = 1)

coef(gnb)[c(TRUE,FALSE)] # Only means

# Visualise class conditional Gaussian distributions
plot(gnb, which = "Sepal.Width", prob = "conditional")

# Check the equivalence of the class conditional distributions
all(get_cond_dist(gnb) == get_cond_dist(nb))
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{predict.gaussian_naive_bayes}}, \code{\link{plot.gaussian_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}
}

