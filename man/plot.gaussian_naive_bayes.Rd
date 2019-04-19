\name{plot.gaussian_naive_bayes}
\alias{plot.gaussian_naive_bayes}
\title{Plot Method for gaussian_naive_bayes objects}
\usage{
\method{plot}{gaussian_naive_bayes}(x, which = NULL, ask = FALSE, legend = TRUE,
  legend.box = FALSE, arg.num = list(), ...)
}
\arguments{
\item{x}{object of class inheriting from \code{"gaussian_naive_bayes"}.}

\item{which}{variables to be plotted (all by default). This can be any valid indexing vector or vector containing names of variables.}

\item{ask}{logical; if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask=.)}.}

\item{legend}{logical; if \code{TRUE} a \code{\link{legend}} will be be plotted.}

\item{legend.box}{logical; if \code{TRUE} a box will be drawn around the legend.}

\item{arg.num}{other parameters to be passed as a named list to \code{\link[graphics]{matplot}}.}

\item{...}{not used.}
}
\description{
Plot method for objects of class \code{"gaussian_naive_bayes"} designed for a quick look at the class conditional Gaussian distributions of metric predictors.
}
\details{
Class conditional Gaussian distributions are visualised by \code{\link[graphics]{matplot}}.
}
\examples{

data(iris)
y <- iris[[5]]
M <- as.matrix(iris[-5])

### Train the Gaussian Naive Bayes
gnb <- gaussian_naive_bayes(x = M, y = y)

# Visualize class conditional Gaussian distributions corresponding
# to the first feature
plot(gnb, which = 1)

}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{predict.naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}
