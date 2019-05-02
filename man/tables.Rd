\name{tables}
\alias{tables}
\title{Browse Tables of Naive Bayes Classifier}
\usage{
tables(object, which = NULL)
}
\arguments{
\item{object}{object of class inheriting from: \code{"naive_bayes"}, \code{"bernoulli_naive_bayes"}.}

\item{which}{tables to be showed (all by default). This can be any valid indexing vector or vector containing names of variables.}
}
\value{
list with tables.
}
\description{
Auxiliary function for \code{"naive_bayes"} and \code{"*_naive_bayes"} objects for easy browsing tables.
}
\details{
Default print method for \code{"naive_bayes"} and \code{"*_naive_bayes"} objects shows at most five first tables. This auxiliary function \code{tables} returns by default all tables and allows easy subsetting via indexing variables.
}
\examples{
data(iris)
nb <- naive_bayes(Species ~ ., data = iris)
tables(nb, "Sepal.Length")
tables(nb, c("Sepal.Length", "Sepal.Width"))
tabs <- tables(nb, 1:2)
tabs
tabs[1]
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{bernoulli_naive_bayes}}, \code{\link{multinomial_naive_bayes}}, \code{\link{poisson_naive_bayes}}, \code{\link{gaussian_naive_bayes}}, \code{\link{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}

