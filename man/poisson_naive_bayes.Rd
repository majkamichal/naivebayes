\name{poisson_naive_bayes}
\alias{poisson_naive_bayes}

\title{Poisson Naive Bayes Classifier}
\usage{
poisson_naive_bayes(x, y, prior = NULL, laplace = 0, ...)
}
\arguments{
\item{x}{matrix with non-negative integer predictors (only numeric matrix is accepted).}

\item{y}{class vector (character/factor/logical).}

\item{prior}{vector with prior probabilities of the classes. If unspecified, the class proportions for the training set are used. If present, the probabilities should be specified in the order of the factor levels.}

\item{laplace}{value used for Laplace smoothing (additive smoothing). Defaults to 0 (no Laplace smoothing).}

\item{...}{not used.}

}

\value{
\code{poisson_naive_bayes} returns an object of class \code{"poisson_naive_bayes"} which is a list with following components:
    \item{data}{list with two components: \code{x} (matrix with predictors) and \code{y} (class variable).}
    \item{levels}{character vector with values of the class variable.}
    \item{laplace}{amount of Laplace smoothing (additive smoothing).}
    \item{params}{matrix containing the class conditional means.}
    \item{prior}{numeric vector with prior probabilities.}
    \item{call}{the call that produced this object.}
}

\description{
\code{poisson_naive_bayes} is used to fit the Poisson Naive Bayes model in which all class conditional distributions are assumed to be Poisson and be independent.
}

\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on non-negative integers (numeric/integer) and class conditional probabilities are modelled with the Poisson distribution.

The Poisson Naive Bayes is available in both, \code{naive_bayes} and \code{poisson_naive_bayes}. The implementation of the specialized Naive Bayes provides more efficient performance though. The speedup comes from the restricting the data input to a numeric matrix and performing the linear algebra as well vectorized operations on it.

The \code{poisson_naive_bayes} function is equivalent to the \code{naive_bayes} function when the numeric matrix or a data.frame contains only non-negative integer variables and each is of class "integer".

The missing values (NAs) are omitted during the parameter estimation. The NAs in the newdata in \code{predict.poisson_naive_bayes} are not included into the calculation of posterior probabilities; and if present an informative warning is given.
}

\note{
When the parameter \code{laplace} is set to positive constant \code{c} then this amount is added to all counts. This leads to the ("global") Bayesian estimation with an improper prior. In each case, the estimate is the expected value of the posterior which is given by the gamma distribution with parameters: \code{cell count + c} and number of observations in the cell.

If in one cell there is a zero count and laplace = 0 then one pseudo-count is automatically to each such cell. This corresponds to the "local" Bayesian estimation with uniform prior.
}


\examples{
### Simulate data
cols <- 10 ; rows <- 100
M <- matrix(rpois(rows * cols, lambda = 3), nrow = rows, ncol = cols)
# is.integer(M) # [1] TRUE
y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE))
colnames(M) <- paste0("V", seq_len(ncol(M)))
laplace <- 0

### Train the Poisson Naive Bayes
pnb <- poisson_naive_bayes(x = M, y = y, laplace = laplace)
summary(pnb)
head(predict(pnb, newdata = M, type = "prob"))

###  Equivalent calculation with general naive_bayes function.
nb <- naive_bayes(M, y, usepoisson = TRUE, laplace = laplace)
head(predict(nb, type = "prob"))

# Obtain probability tables
tables(pnb, which = 1)
tables(nb, "V1")
coef(pnb)

# Visualise class conditional Poisson distributions
plot(pnb, which = "V1", prob = "conditional")

# Visualise class marginal distributions
plot(pnb, which = "V1", prob = "marginal")

# Check the equivalence of the class conditional distributions
all(get_cond_dist(pnb) == get_cond_dist(nb))
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{predict.poisson_naive_bayes}}, \code{\link{plot.poisson_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}, \code{\link[naivebayes]{coef.poisson_naive_bayes}}
}

