\name{nonparametric_naive_bayes}
\alias{nonparametric_naive_bayes}

\title{Non-Parametric Naive Bayes Classifier}
\usage{
nonparametric_naive_bayes(x, y, prior = NULL, ...)
}
\arguments{
\item{x}{matrix with metric predictors (only numeric matrix accepted).}

\item{y}{class vector (character/factor/logical).}

\item{prior}{vector with prior probabilities of the classes. If unspecified, the class proportions for the training set are used. If present, the probabilities should be specified in the order of the factor levels.}

\item{...}{other parameters to \code{\link[stats]{density}} (for instance \code{adjust}, \code{kernel} or \code{bw}).}

}

\value{
\code{nonparametric_naive_bayes} returns an object of class \code{"nonparametric_naive_bayes"} which is a list with following components:
    \item{data}{list with two components: \code{x} (matrix with predictors) and \code{y} (class variable).}
    \item{levels}{character vector with values of the class variable.}
    \item{dens}{nested list containing \code{\link[stats]{density}} objects for each feature and class.}
    \item{prior}{numeric vector with prior probabilities.}
    \item{call}{the call that produced this object.}
}
\description{
\code{nonparametric_naive_bayes} is used to fit the Non-Parametric Naive Bayes model in which all class conditional distributions are non-parametrically estimated using kernel density estimator and are assumed to be independent.
}

\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on real values (numeric/integer) and class conditional probabilities are estimated in a non-parametric way with the kernel density estimator (KDE). By default Gaussian kernel is used and the smoothing bandwidth is selected according to the Silverman's 'rule of thumb'. For more details, please see the references and the documentation of \code{\link{density}} and \code{\link{bw.nrd0}}.

The Non-Parametric Naive Bayes is available in both, \code{naive_bayes()} and \code{nonparametric_naive_bayes()}. The latter does not provide a substantial speed up over the general \code{naive_bayes()} function but it is meant to be more transparent and user friendly.

The \code{nonparametric_naive_bayes} and \code{naive_bayes()} are equivalent when the latter is used with \code{usekernel = TRUE} and \code{usepoisson = FALSE}; and a matrix/data.frame contains only numeric variables.

The missing values (NAs) are omitted during the estimation process. Also, the corresponding predict function excludes all NAs from the calculation of posterior probabilities  (an informative warning is always given).
}

\references{
Silverman, B. W. (1986). Density Estimation for Statistics and Data Analysis. Chapman & Hall.
}

\examples{
# library(naivebayes)
data(iris)
y <- iris[[5]]
M <- as.matrix(iris[-5])

### Train the Non-Parametric Naive Bayes
nnb <- nonparametric_naive_bayes(x = M, y = y)
head(predict(nnb, newdata = M, type = "prob"))

###  Equivalent calculation with general naive_bayes function:
nb <- naive_bayes(M, y, usekernel = TRUE)
head(predict(nb, type = "prob"))

### Change kernel
nnb_kernel <- nonparametric_naive_bayes(x = M, y = y, kernel = "biweight")
plot(nnb_kernel, 1, prob = "conditional")

### Adjust bandwidth
nnb_adjust <- nonparametric_naive_bayes(M, y, adjust = 1.5)
plot(nnb_adjust, 1, prob = "conditional")

### Change bandwidth selector
nnb_bw <- nonparametric_naive_bayes(M, y, bw = "SJ")
plot(nnb_bw, 1, prob = "conditional")

### Obtain tables with conditional densities
# tables(nnb, which = 1)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{predict.nonparametric_naive_bayes}}, \code{\link{plot.nonparametric_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}
}
