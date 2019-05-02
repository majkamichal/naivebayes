\name{plot.nonparametric_naive_bayes}
\alias{plot.nonparametric_naive_bayes}
\title{Plot Method for nonparametric_naive_bayes Objects}
\usage{
\method{plot}{nonparametric_naive_bayes}(x, which = NULL, ask = FALSE, legend = TRUE,
  legend.box = FALSE, arg.num = list(),
  prob = c("marginal", "conditional"), ...)
}
\arguments{
\item{x}{object of class inheriting from \code{"nonparametric_naive_bayes"}.}

\item{which}{variables to be plotted (all by default). This can be any valid indexing vector or vector containing names of variables.}

\item{ask}{logical; if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask=.)}.}

\item{legend}{logical; if \code{TRUE} a \code{\link{legend}} will be be plotted.}

\item{legend.box}{logical; if \code{TRUE} a box will be drawn around the legend.}

\item{arg.num}{other parameters to be passed as a named list to \code{\link[graphics]{matplot}}.}

\item{prob}{character; if "marginal" then marginal distributions of predictor variables for each class are visualised and if "conditional" then the class conditional distributions of predictor variables are depicted. By default, prob="marginal".}

\item{...}{not used.}
}
\description{
Plot method for objects of class \code{"nonparametric_naive_bayes"} designed for a quick look at the estimated class marginal or class conditional densities of metric predictors.
}
\details{
Estimated class marginal or class conditional densities are visualised by \code{\link[graphics]{matplot}}.

The parameter \code{prob} controls the kind of probabilities to be visualized for each individual predictor \eqn{Xi}. It can take on two values:
\itemize{
    \item "marginal": \eqn{P(Xi|class) * P(class)}
    \item "conditional": \eqn{P(Xi|class)}
}

}
\examples{
data(iris)
y <- iris[[5]]
M <- as.matrix(iris[-5])

### Train the Non-Parametric Naive Bayes with custom prior
prior <- c(0.1,0.3,0.6)
nnb <- nonparametric_naive_bayes(x = M, y = y, prior = prior)
nnb2 <- nonparametric_naive_bayes(x = M, y = y, prior = prior, adjust = 1.5)
nnb3 <- nonparametric_naive_bayes(x = M, y = y, prior = prior, bw = "ucv")

# Visualize estimated class conditional densities corresponding
# to the first feature
plot(nnb, which = 1, prob = "conditional")
plot(nnb2, which = 1, prob = "cond")
plot(nnb3, which = 1, prob = "c")

# Visualize estimated class marginal densities corresponding
# to the first feature
plot(nnb, which = 1)
plot(nnb2, which = 1)
plot(nnb3, which = 1)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{nonparametric_naive_bayes}} \code{\link{predict.nonparametric_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}
