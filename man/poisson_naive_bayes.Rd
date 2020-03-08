\name{poisson_naive_bayes}
\alias{poisson_naive_bayes}

\title{Poisson Naive Bayes Classifier}
\usage{
poisson_naive_bayes(x, y, prior = NULL, laplace = 0, ...)
}
\arguments{
\item{x}{numeric matrix with integer predictors (matrix or dgCMatrix from Matrix package).}

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
    \item{params}{matrix containing class conditional means.}
    \item{prior}{numeric vector with prior probabilities.}
    \item{call}{the call that produced this object.}
}

\description{
\code{poisson_naive_bayes} is used to fit the Poisson Naive Bayes model in which all class conditional distributions are assumed to be Poisson and be independent.
}

\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on non-negative integers (numeric/integer) and class conditional probabilities are modelled with the Poisson distribution.

The Poisson Naive Bayes is available in both, \code{naive_bayes} and \code{poisson_naive_bayes}. The latter provides more efficient performance though. Faster calculation times come from restricting the data to an integer-valued matrix and taking advantage of linear algebra operations. Sparse matrices of class "dgCMatrix" (Matrix package) are supported in order to furthermore speed up calculation times.

The \code{poisson_naive_bayes} and \code{naive_bayes()} are equivalent when the latter is used with \code{usepoisson = TRUE} and \code{usekernel = FALSE}; and a matrix/data.frame contains only integer-valued columns.

The missing values (NAs) are omited during the estimation process. Also, the corresponding predict function excludes all NAs from the calculation of posterior probabilities  (an informative warning is always given).
}

\note{
When the parameter \code{laplace} is set to positive constant \code{c} then this amount is added to all counts. This leads to the ("global") Bayesian estimation with an improper prior. In each case, the estimate is the expected value of the posterior which is given by the gamma distribution with parameters: \code{cell count + c} and number of observations in the cell.

If in one cell there is a zero count and \code{laplace = 0} then one pseudo-count is automatically to each such cell. This corresponds to the "local" Bayesian estimation with uniform prior.
}


\examples{
library(naivebayes)

### Simulate the data:
cols <- 10 ; rows <- 100
M <- matrix(rpois(rows * cols, lambda = 3), nrow = rows, ncol = cols)
y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE, prob = c(0.3,0.7)))
colnames(M) <- paste0("V", seq_len(ncol(M)))
laplace <- 0.5


### Train the Poisson Naive Bayes
pnb <- poisson_naive_bayes(x = M, y = y, laplace = laplace)
summary(pnb)

# Classification
head(predict(pnb, newdata = M, type = "class")) # head(pnb \%class\% M)

# Posterior probabilities
head(predict(pnb, newdata = M, type = "prob")) # head(pnb \%prob\% M)

# Parameter estimates
coef(pnb)


### Sparse data: train the Poisson Naive Bayes
library(Matrix)
M_sparse <- Matrix(M, sparse = TRUE)
class(M_sparse) # dgCMatrix

# Fit the model with sparse data
pnb_sparse <- poisson_naive_bayes(M_sparse, y, laplace = laplace)

# Classification
head(predict(pnb_sparse, newdata = M_sparse, type = "class"))

# Posterior probabilities
head(predict(pnb_sparse, newdata = M_sparse, type = "prob"))

# Parameter estimates
coef(pnb_sparse)


### Equivalent calculation with general naive_bayes function.
### (no sparse data support by naive_bayes)

nb <- naive_bayes(M, y, laplace = laplace, usepoisson = TRUE)
summary(nb)
head(predict(nb, type = "prob"))

# Obtain probability tables
tables(nb, which = "V1")
tables(pnb, which = "V1")

# Visualise class conditional Poisson distributions
plot(nb, "V1", prob = "conditional")
plot(pnb, which = "V1", prob = "conditional")

# Check the equivalence of the class conditional distributions
all(get_cond_dist(nb) == get_cond_dist(pnb))
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{predict.poisson_naive_bayes}}, \code{\link{plot.poisson_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}, \code{\link[naivebayes]{coef.poisson_naive_bayes}}
}

