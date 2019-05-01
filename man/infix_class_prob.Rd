\name{Infix operators}
\alias{\%class\%}
\alias{\%prob\%}
\title{Predict Method for Family of Naive Bayes Objects}

\usage{
lhs \%class\% rhs
lhs \%prob\% rhs
}

\arguments{
\item{lhs}{object of class inheriting from \code{"naive_bayes"} and \code{"*_naive_bayes"} family.}

\item{rhs}{dataframe or matrix for "naive_bayes" objects OR matrix for all "*_naive_bayes" objects.}
}

\value{
\itemize{
    \item \code{\%class\%} returns factor with class labels corresponding to the maximal conditional posterior probabilities.
    \item \code{\%prob\%} returns a matrix with class label specific conditional posterior probabilities.
}
}
\description{
The infix operators \code{\%class\%} and \code{\%prob\%} are shorthands for performing classification and obtaining posterior probabilities, respectively.
}
\details{

If \code{lhs} is of class inheriting from the family of the Naive Bayes objects and \code{rhs} is either dataframe or matrix then the infix operators \code{\%class\%} and \code{\%prob\%} are equivalent to:
\itemize{
    \item \code{lhs \%class\% rhs} <=> \code{predict(lhs, newdata = rhs, type = "class")}
    \item \code{lhs \%prob\% rhs} <=> \code{predict(lhs, newdata = rhs, type == "prob")}
}
}
\examples{
### Fit the model
newdata <- iris[1:5,-5] # Let's pretend
nb <- naive_bayes(Species ~ ., iris)

### Classification
nb \%class\% newdata
predict(nb, newdata, type = "class")

### Posterior probabilities
nb \%prob\% newdata
predict(nb, newdata, type = "prob")


}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{predict.naive_bayes}}, \code{\link{predict.bernoulli_naive_bayes}}, \code{\link{predict.multinomial_naive_bayes}}, \code{\link{predict.poisson_naive_bayes}}, \code{\link{predict.gaussian_naive_bayes}}, \code{\link{predict.nonparametric_naive_bayes}}
}

