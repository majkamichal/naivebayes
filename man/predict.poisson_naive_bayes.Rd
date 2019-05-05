\name{predict.poisson_naive_bayes}
\alias{predict.poisson_naive_bayes}
\title{Predict Method for poisson_naive_bayes Objects}
\usage{
\method{predict}{poisson_naive_bayes}(object, newdata = NULL, type = c("class","prob"),
  threshold = 0.001, eps = 0, ...)
}
\arguments{
\item{object}{object of class inheriting from \code{"poisson_naive_bayes"}.}

\item{newdata}{matrix with non-negative integer predictors (only numeric matrix is accepted).}

\item{type}{if "class", new data points are classified according to the highest posterior probabilities. If "prob", the posterior probabilities for each class are returned.}

\item{threshold}{value by which zero probabilities or probabilities within the epsilon-range corresponding to metric variables are replaced (zero probabilities corresponding to categorical variables can be handled with Laplace (additive) smoothing).}

\item{eps}{value that specifies an epsilon-range to replace zero or close to zero probabilities by \code{threshold}.}

\item{...}{not used.}
}
\value{
\code{predict.poisson_naive_bayes} returns either a factor with class labels corresponding to the maximal conditional posterior probabilities or a matrix with class label specific conditional posterior probabilities.
}
\description{
Classification based on the Poisson Naive Bayes model.
}
\details{

This is a specialized version of the Naive Bayes classifier, in which all features are non-negative integers and class conditional probabilities are modelled with the Poisson distribution.

Class posterior probabilities are calculated using the Bayes' rule under the assumption of independence of predictors. If no \code{newdata} is provided, the data from the object is used.

The Poisson Naive Bayes is available in both, \code{naive_bayes} and \code{poisson_naive_bayes}. The implementation of the specialized Naive Bayes provides more efficient performance though. The speedup comes from the restricting the data input to a numeric matrix and performing the linear algebra as well vectorized operations on it.

The NAs in the newdata are not included into the calculation of posterior probabilities; and if present an informative warning is given.

The \code{poisson_naive_bayes} function is equivalent to the \code{naive_bayes} function with \code{usepoisson=TRUE} and a numeric matrix or a data.frame containing only non-negative integer valued features (each variable has class "integer").

}
\examples{
cols <- 10 ; rows <- 100
M <- matrix(rpois(rows * cols, lambda = 3), nrow = rows, ncol = cols)
# is.integer(M) # [1] TRUE
y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE))
colnames(M) <- paste0("V", seq_len(ncol(M)))
laplace <- 0

### Train the Poisson Naive Bayes
pnb <- poisson_naive_bayes(x = M, y = y, laplace = laplace)

### Classification
head(predict(pnb, newdata = M, type = "class"))
head(pnb \%class\% M)

### Posterior probabilities
head(predict(pnb, newdata = M, type = "prob"))
head(pnb \%prob\% M)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{poisson_naive_bayes}}, \code{\link{plot.poisson_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}, \code{\link[naivebayes]{coef.poisson_naive_bayes}}
}
