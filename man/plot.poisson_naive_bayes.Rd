\name{plot.poisson_naive_bayes}
\alias{plot.poisson_naive_bayes}
\title{Plot Method for poisson_naive_bayes Objects}
\usage{
\method{plot}{poisson_naive_bayes}(x, which = NULL, ask = FALSE, legend = TRUE,
  legend.box = FALSE, arg.num = list(),
  prob = c("marginal", "conditional"), ...)
}
\arguments{
\item{x}{object of class inheriting from \code{"poisson_naive_bayes"}.}

\item{which}{variables to be plotted (all by default). This can be any valid indexing vector or vector containing names of variables.}

\item{ask}{logical; if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask=.)}.}

\item{legend}{logical; if \code{TRUE} a \code{\link{legend}} will be be plotted.}

\item{legend.box}{logical; if \code{TRUE} a box will be drawn around the legend.}

\item{arg.num}{other parameters to be passed as a named list to \code{\link[graphics]{matplot}}.}

\item{prob}{character; if "marginal" then marginal distributions of predictor variables for each class are visualised and if "conditional" then the class conditional distributions of predictor variables are depicted. By default, prob="marginal".}

\item{...}{not used.}
}
\description{
Plot method for objects of class \code{"poisson_naive_bayes"} designed for a quick look at the class marginal or class conditional Poisson distributions of non-negative integer predictors.
}
\details{
Class marginal or class conditional Poisson distributions are visualised by \code{\link[graphics]{matplot}}.

The parameter \code{prob} controls the kind of probabilities to be visualized for each individual predictor \eqn{Xi}. It can take on two values:
\itemize{
    \item "marginal": \eqn{P(Xi|class) * P(class)}
    \item "conditional": \eqn{P(Xi|class)}
}
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

# Visualize class conditional Poisson distributions corresponding
# to the first feature
plot(pnb, which = 1, prob = "conditional")

# Visualize class marginal Poisson distributions corresponding
# to the first feature
plot(pnb, which = 1)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{poisson_naive_bayes}}, \code{\link{predict.poisson_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}
