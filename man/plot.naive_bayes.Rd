\name{plot.naive_bayes}
\alias{plot.naive_bayes}
\title{Plot Method for naive_bayes Objects}
\usage{
\method{plot}{naive_bayes}(x, which = NULL, ask = FALSE, legend = TRUE,
  legend.box = FALSE, arg.num = list(), arg.cat = list(),
  prob = c("marginal", "conditional"), ...)
}
\arguments{
\item{x}{object of class inheriting from \code{"naive_bayes"}.}

\item{which}{variables to be plotted (all by default). This can be any valid indexing vector or vector containing names of variables.}

\item{ask}{logical; if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask=.)}.}

\item{legend}{logical; if \code{TRUE} a \code{\link{legend}} will be be plotted.}

\item{legend.box}{logical; if \code{TRUE} a box will be drawn around the legend.}

\item{arg.num}{other parameters to be passed as a named list to \code{\link[graphics]{matplot}}.}

\item{arg.cat}{other parameters to be passed as a named list to \code{\link[graphics]{mosaicplot}}.}

\item{prob}{character; if "marginal" then marginal distributions of predictor variables for each class are visualised and if "conditional" then the class conditional distributions of predictor variables are depicted. By default, prob="marginal".}

\item{...}{not used.}
}
\description{
Plot method for objects of class \code{"naive_bayes"} designed for a quick look at the class marginal distributions or class conditional distributions of predictor variables.
}
\details{
Probabilities are visualised by \code{\link[graphics]{matplot}} (for numeric (metric) predictors) and  \code{\link[graphics]{mosaicplot}} (for categorical predictors). In case of non parametric estimation of densities, the bandwidths are reported for each class. Nothing is returned. For numeric (metric) predictors position of the legend can be adjusted changed via \code{arg.num(..., legend.position = "topright")}. \code{legend.position} can be one of \code{"topright" "topleft", "bottomright", "bottomleft"}. In order to adjust the legend size following argument can be used: \code{arg.num(..., legend.cex = 0.9)}.


The parameter \code{prob} controls the kind of probabilities to be visualized for each individual predictor \eqn{Xi}. It can take on two values:
\itemize{
    \item "marginal": \eqn{P(Xi|class) * P(class)}
    \item "conditional": \eqn{P(Xi|class)}
}



}
\examples{
data(iris)
iris2 <- cbind(iris, New = sample(letters[1:3], 150, TRUE))

# Fit the model with custom prior probabilities
nb <- naive_bayes(Species ~ ., data = iris2, prior = c(0.1, 0.3, 0.6))

# Visualize marginal distributions of two predictors
plot(nb, which = c("Sepal.Width", "Sepal.Length"), ask = TRUE)

# Visualize class conditional distributions corresponding to the first predictor
# with customized settings
plot(nb, which = 1, ask = FALSE, prob = "conditional",
     arg.num = list(col = 1:3, lty = 1,
     main = "Naive Bayes Plot", legend.position = "topright",
     legend.cex = 0.55))

# Visualize class marginal distributions corresponding to the first predictor
# with customized settings
plot(nb, which = 1, ask = FALSE, prob = "marginal",
     arg.num = list(col = 1:3, lty = 1,
     main = "Naive Bayes Plot", legend.position = "topright",
     legend.cex = 0.55))

# Visualize class marginal distribution corresponding to the predictor "new"
# with custom colours
plot(nb, which = "New", arg.cat = list(color = gray.colors(3)))
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{predict.naive_bayes}}, \code{\link{\%class\%}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}

