\name{predict.bernoulli_naive_bayes}
\alias{predict.bernoulli_naive_bayes}
\title{Predict Method for bernoulli_naive_bayes Objects}
\usage{
\method{predict}{bernoulli_naive_bayes}(object, newdata = NULL, type = c("class","prob"), ...)
}
\arguments{
\item{object}{object of class inheriting from \code{"bernoulli_naive_bayes"}.}

\item{newdata}{matrix with numeric 0-1 predictors.}

\item{type}{if "class", new data points are classified according to the highest posterior probabilities. If "prob", the posterior probabilities for each class are returned.}

\item{...}{not used.}
}
\value{
\code{predict.bernoulli_naive_bayes} returns either a factor with class labels corresponding to the maximal conditional posterior probabilities or a matrix with class label specific conditional posterior probabilities.
}
\description{
Classification based on the Bernoulli Naive Bayes model.
}
\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on numeric 0-1 values and class conditional probabilities are modelled with the Bernoulli distribution.

Class posterior probabilities are calculated using the Bayes' rule under the assumption of independence of predictors. If no \code{newdata} is provided, the data from the object is used.

The Bernoulli Naive Bayes is available in both, \code{naive_bayes} and \code{bernoulli_naive_bayes}. The implementation of the specialized Naive Bayes provides more efficient performance though. The speedup comes from the restricting the data input to a numeric 0-1 matrix and performing the linear algebra as well as vectorized operations on it. In other words, the efficiency comes at cost of the flexibility.

The NAs in the newdata are not included into the calculation of posterior probabilities; and if present an informative warning is given.

The \code{bernoulli_naive_bayes} function is equivalent to the \code{naive_bayes} function with the numeric 0-1 matrix being coerced, for instance,  to the "0"-"1" character matrix.
}
\examples{
cols <- 10 ; rows <- 100 ; probs <- c("0" = 0.4, "1" = 0.1)
M <- matrix(sample(0:1, rows * cols,  TRUE, probs), nrow = rows, ncol = cols)
y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE, prob = c(0.3,0.7)))
colnames(M) <- paste0("V", seq_len(ncol(M)))
laplace <- 0.5

### Train the Bernoulli Naive Bayes
bnb <- bernoulli_naive_bayes(x = M, y = y, laplace = laplace)

### Classification
head(predict(bnb, newdata = M, type = "class"))
head(bnb \%class\% M)

### Posterior probabilities
head(predict(bnb, newdata = M, type = "prob"))
head(bnb \%prob\% M)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{bernoulli_naive_bayes}}, \code{\link{plot.bernoulli_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}
}
