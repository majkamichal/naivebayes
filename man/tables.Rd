\name{tables}
\alias{tables}
\title{Browse Tables of Naive Bayes Classifier}
\usage{
tables(object, which = NULL)
}
\arguments{
\item{object}{object of class inheriting from \code{"naive_bayes"}.}

\item{which}{tables to be showed (all by default). This can be any valid indexing vector or vector containing names of variables.}
}
\value{
list with tables.
}
\description{
Auxiliary function for \code{"naive_bayes"} objects for easy browsing tables.
}
\details{
Default print method for \code{"naive_bayes"} objects shows at most five first tables. The auxiliary function \code{tables} returns by default all tables.
}
\examples{
data(iris)
nb <- naive_bayes(Species ~ ., data = iris)
tables(nb, "Sepal.Length")
tables(nb, 1:2)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{predict.naive_bayes}}, \code{\link{plot.naive_bayes}}
}

