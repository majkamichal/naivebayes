\name{plot.naive_bayes}
\alias{plot.naive_bayes}
\title{Plot Method for naive_bayes Object}
\usage{
\method{plot}{naive_bayes}(x, which = NULL, ask = FALSE, legend = TRUE,
  legend.box = FALSE, arg.num = list(), arg.cat = list(), ...)
}
\arguments{
\item{x}{object of class inheriting from \code{"naive_bayes"}.}

\item{which}{variables to be plotted (all by default). This can be any valid indexing vector or vector containing names of variables.}

\item{ask}{logical; if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask=.)}.}

\item{legend}{logical; if \code{TRUE} a \code{\link{legend}} will be be plotted.}

\item{legend.box}{logical; if \code{TRUE} a box will be drawn around the legend.}

\item{arg.num}{other parameters to be passed as a named list to \code{\link[graphics]{matplot}}.}

\item{arg.cat}{other parameters to be passed as a named list to \code{\link[graphics]{mosaicplot}}.}

\item{...}{not used.}
}
\description{
Plot method for objects of class \code{"naive_bayes"} designed for a quick look at the class conditional distributions of predictor variables.
}
\details{
Conditional probabilities are visualised by \code{\link[graphics]{matplot}} (for numeric (metric) predictors) and  \code{\link[graphics]{mosaicplot}} (for categorical predictors). In case of non parametric estimation of class-conditional densities, the bandwidths are reported. Nothing is returned. For numeric (metric) predictors position of the legend can be adjusted changed via \code{arg.num(..., legend.position = "topright")}. \code{legend.position} can be one of \code{"topright" "topleft", "bottomright", "bottomleft"}.
}
\examples{
data(iris)
iris2 <- cbind(iris, New = sample(letters[1:3], 150, TRUE))
nb <- naive_bayes(Species ~ ., data = iris2)

plot(nb, which = c("Sepal.Width", "Sepal.Length"), ask = TRUE)

plot(nb, which = c(1, 2), ask = FALSE,
     arg.num = list(col = 1:3, lty = 1,
     main = "Naive Bayes Plot", legend.position = "topright",
     legend.cex = 0.55))

plot(nb, which = "New", arg.cat = list(color = heat.colors(3)))
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{predict.naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}

