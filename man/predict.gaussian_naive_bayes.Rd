\name{predict.gaussian_naive_bayes}
\alias{predict.gaussian_naive_bayes}
\title{Predict Method for gaussian_naive_bayes Objects}
\usage{
\method{predict}{gaussian_naive_bayes}(object, newdata = NULL, type = c("class","prob"),
  threshold = 0.001, eps = 0, ...)
}
\arguments{
\item{object}{object of class inheriting from \code{"gaussian_naive_bayes"}.}

\item{newdata}{matrix with metric predictors (only numeric matrix accepted).}

\item{type}{if "class", new data points are classified according to the highest posterior probabilities. If "prob", the posterior probabilities for each class are returned.}

\item{threshold}{value by which zero probabilities or probabilities within the epsilon-range corresponding to metric variables are replaced (zero probabilities corresponding to categorical variables can be handled with Laplace (additive) smoothing).}

\item{eps}{value that specifies an epsilon-range to replace zero or close to zero probabilities by \code{threshold}. It applies to metric variables.}

\item{...}{not used.}
}
\value{
\code{predict.gaussian_naive_bayes} returns either a factor with class labels corresponding to the maximal conditional posterior probabilities or a matrix with class label specific conditional posterior probabilities.
}
\description{
Classification based on the Gaussian Naive Bayes model.
}
\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on real values and class conditional probabilities are modelled with the Gaussian distribution.

Class posterior probabilities are calculated using the Bayes' rule under the assumption of independence of predictors. If no \code{newdata} is provided, the data from the object is used.

The Gaussian Naive Bayes is available in both, \code{naive_bayes} and \code{gaussian_naive_bayes}. The implementation of the specialized Naive Bayes provides more efficient performance though. The speedup comes from the restricting the data input to a numeric matrix and performing the linear algebra as well vectorized operations on it. In other words, the efficiency comes at cost of the flexibility.

The NAs in the newdata are not included into the calculation of posterior probabilities; and if present an informative warning is given.

The \code{gaussian_naive_bayes} function is equivalent to the \code{naive_bayes} function with the numeric matrix or a data.frame containing only numeric variables.

}
\examples{
data(iris)
y <- iris[[5]]
M <- as.matrix(iris[-5])

### Train the Gaussian Naive Bayes
gnb <- gaussian_naive_bayes(x = M, y = y)

### Classification
head(predict(gnb, newdata = M, type = "class"))
head(gnb \%class\% M)

### Posterior probabilities
head(predict(gnb, newdata = M, type = "prob"))
head(gnb \%prob\% M)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{gaussian_naive_bayes}},  \code{\link{plot.gaussian_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}
}
