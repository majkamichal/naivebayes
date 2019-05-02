\name{get_cond_dist}
\alias{get_cond_dist}
\title{Obtain names of class conditional distribution assigned to features}
\usage{
get_cond_dist(object)
}
\arguments{
\item{object}{object of class inheriting from \code{"naive_bayes"} or \code{"*_naive_bayes"} or \code{"naive_bayes_tables"}.}
}
\value{
vector with names of class conditional distributions assigned to the features.
}
\description{
Auxiliary function for \code{"naive_bayes"}, \code{"*_naive_bayes"} and \code{"naive_bayes_tables"}  objects for obtaining names of class conditional distributions assigned to the features.
}
\examples{
data(iris)
nb <- naive_bayes(Species ~ ., data = iris)
get_cond_dist(nb) # <=> attr(nb$tables, "cond_dist")
get_cond_dist(tables(nb))
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{bernoulli_naive_bayes}}, \code{\link{multinomial_naive_bayes}}, \code{\link{poisson_naive_bayes}}, \code{\link{gaussian_naive_bayes}}, \code{\link{tables}}
}

