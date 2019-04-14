\name{bernoulli_tables_to_df}
\alias{bernoulli_tables_to_df}
\title{Coerce Bernoulli probability tables to the data.frame}
\usage{
bernoulli_tables_to_df(object)
}
\arguments{
\item{object}{object of class inheriting from \code{"bernoulli_naive_bayes"}.}
}
\value{
data.frame with class conditional Bernoulli probabilities.
}
\description{
Auxiliary function for \code{"bernoulli_naive_bayes"} objects for extracting and coercing the Bernoulli probability tables to the data.frame.
}
\details{
Default print method for \code{"naive_bayes"} objects shows at most five first tables. This auxiliary function \code{tables} returns by default all tables.
}
\examples{

# Simulate data
cols <- 10 ; rows <- 100 ; probs <- c("0" = 0.4, "1" = 0.1)
M <- matrix(sample(0:1, rows * cols,  TRUE, probs), nrow = rows, ncol = cols)
y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE, prob = c(0.3,0.7)))
colnames(M) <- paste0("V", seq_len(ncol(M)))
laplace <- 0.5

# Fit the Bernoulli Naive Bayes model
bnb <- bernoulli_naive_bayes(x = M, y = y, laplace = laplace)

# Extract the probability tables and convert them to the data.frame
bernoulli_tables_to_df(bnb)

# Compare first row with the first table:
tables(bnb, which = 2)
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{predict.naive_bayes}}, \code{\link{plot.naive_bayes}}, \code{\link[naivebayes]{get_cond_dist}}
}

