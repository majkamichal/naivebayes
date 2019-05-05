\name{predict.nonparametric_naive_bayes}
\alias{predict.nonparametric_naive_bayes}
\title{Predict Method for nonparametric_naive_bayes Objects}
\usage{
\method{predict}{nonparametric_naive_bayes}(object, newdata = NULL, type = c("class","prob"),
  threshold = 0.001, eps = 0, ...)
}
\arguments{
\item{object}{object of class inheriting from \code{"nonparametric_naive_bayes"}.}

\item{newdata}{matrix with metric predictors (only numeric matrix accepted).}

\item{type}{if "class", new data points are classified according to the highest posterior probabilities. If "prob", the posterior probabilities for each class are returned.}

\item{threshold}{value by which zero probabilities or probabilities within the epsilon-range corresponding to metric variables are replaced (zero probabilities corresponding to categorical variables can be handled with Laplace (additive) smoothing).}

\item{eps}{value that specifies an epsilon-range to replace zero or close to zero probabilities by \code{threshold}. It applies to metric variables.}

\item{...}{not used.}
}
\value{
\code{predict.nonparametric_naive_bayes} returns either a factor with class labels corresponding to the maximal conditional posterior probabilities or a matrix with class label specific conditional posterior probabilities.
}
\description{
Classification based on the Non-Parametric Naive Bayes model.
}
\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on real values (numeric/integer) and class conditional probabilities are non-parametrically estimated with kernel density estimator. By default Gaussian kernel is used and the smoothing bandwidth is selected according to the Silverman's 'rule of thumb'. For more details, please see the references and the documentation of \code{\link{density}} and \code{\link{bw.nrd0}}.

The Non-Parametric Naive Bayes is available in both, \code{naive_bayes()} and \code{nonparametric_naive_bayes()}. This specialized implementation of the Naive Bayes does not provide a substantial speed-up over the general \code{naive_bayes()} function but it should be more transparent and user friendly.

The \code{nonparametric_naive_bayes} function is equivalent to \code{naive_bayes()} when the numeric matrix or a data.frame contains only numeric variables and \code{usekernel = TRUE}.

The missing values (NAs) are omitted during the parameter estimation. The NAs in the newdata in \code{predict.nonparametric_naive_bayes()} are not included into the calculation of posterior probabilities; and if present an informative warning is given.
}

\references{
Silverman, B. W. (1986). Density Estimation for Statistics and Data Analysis. Chapman & Hall.
}

\examples{
data(iris)
y <- iris[[5]]
M <- as.matrix(iris[-5])

### Train the Non-Parametric Naive Bayes
nnb <- nonparametric_naive_bayes(x = M, y = y, bw = "SJ")

### Classification
head(predict(nnb, newdata = M, type = "class"))
head(nnb \%class\% M)

### Posterior probabilities
head(predict(nnb, newdata = M, type = "prob"))
head(nnb \%prob\% M)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{nonparametric_naive_bayes}}, \code{\link{plot.nonparametric_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link{naive_bayes}}, \code{\link[naivebayes]{\%class\%}}
}
