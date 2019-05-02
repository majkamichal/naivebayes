\name{plot.bernoulli_naive_bayes}
\alias{plot.bernoulli_naive_bayes}
\title{Plot Method for bernoulli_naive_bayes Objects}
\usage{
\method{plot}{bernoulli_naive_bayes}(x, which = NULL, ask = FALSE, arg.cat = list(),
     prob = c("marginal", "conditional"), ...)
}
\arguments{
\item{x}{object of class inheriting from \code{"bernoulli_naive_bayes"}.}

\item{which}{variables to be plotted (all by default). This can be any valid indexing vector or vector containing names of variables.}

\item{ask}{logical; if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask=.)}.}

\item{arg.cat}{other parameters to be passed as a named list to \code{\link[graphics]{mosaicplot}}.}

\item{prob}{character; if "marginal" then marginal distributions of predictor variables for each class are visualised and if "conditional" then the class conditional distributions of predictor variables are depicted. By default, prob="marginal".}

\item{...}{not used.}
}
\description{
Plot method for objects of class \code{"bernoulli_naive_bayes"} designed for a quick look at the class marginal distributions or class conditional distributions of 0-1 valued predictors.
}
\details{
Class conditional or class conditional distributions are visualised by \code{\link[graphics]{mosaicplot}}.

The parameter \code{prob} controls the kind of probabilities to be visualized for each individual predictor \eqn{Xi}. It can take on two values:
\itemize{
    \item "marginal": \eqn{P(Xi|class) * P(class)}
    \item "conditional": \eqn{P(Xi|class)}
}
}
\examples{
# Simulate data
cols <- 10 ; rows <- 100 ; probs <- c("0" = 0.4, "1" = 0.1)
M <- matrix(sample(0:1, rows * cols,  TRUE, probs), nrow = rows, ncol = cols)
y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE, prob = c(0.3,0.7)))
colnames(M) <- paste0("V", seq_len(ncol(M)))
laplace <- 0.5

# Train the Bernoulli Naive Bayes model
bnb <- bernoulli_naive_bayes(x = M, y = y, laplace = laplace)

# Visualize class marginal probabilities corresponding to the first feature
plot(bnb, which = 1)

# Visualize class conditional probabilities corresponding to the first feature
plot(bnb, which = 1, prob = "conditional")

}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}},\code{\link{bernoulli_naive_bayes}} \code{\link{predict.bernoulli_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}

