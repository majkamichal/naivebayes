\name{predict.bernoulli_naive_bayes}
\alias{predict.bernoulli_naive_bayes}
\title{Predict Method for bernoulli_naive_bayes Objects}
\usage{
\method{predict}{bernoulli_naive_bayes}(object, newdata = NULL, type = c("class","prob"), ...)
}
\arguments{
\item{object}{object of class inheriting from \code{"bernoulli_naive_bayes"}.}

\item{newdata}{matrix with numeric 0-1 predictors.}

\item{type}{if "class", new data points are classified according to the highest posterior probabilities. If "prob", the posterior probabilities for each class are returned.}

\item{...}{not used.}
}
\value{
\code{predict.bernoulli_naive_bayes} returns either a factor with class labels corresponding to the maximal conditional posterior probabilities or a matrix with class label specific conditional posterior probabilities.
}
\description{
Classification based on the Bernoulli Naive Bayes model.
}
\details{
Computes conditional posterior probabilities for each class label using the Bayes' rule under the assumption of independence of predictors. If no new data is provided, the data from the object is used. Currently the \code{predict.bernoulli_naive_bayes} function does not handle missing values.

This is a specialized version of the Naive Bayes classifier, in which all features take on numeric 0-1 values and class conditional probabilities are modelled with the Bernoulli distribution.

The Bernoulli Naive Bayes is available in both, \code{naive_bayes} and \code{bernoulli_naive_bayes} and corresponding \code{predict} functions. The implementation of the specialized Naive Bayes proovides more efficient performance though. The speedup comes from the restricting the data input to a numeric 0-1 matrix and performing the linear algebra operations on it. In other words, the efficiency comes at cost of the flexibility.

The \code{bernoulli_naive_bayes} function is equivalent to the \code{naive_bayes} function when the numeric 0-1 matrix is coerced to, for instance, a 0-1 character matrix.
}
\examples{

### Simulate the data:
cols <- 10 ; rows <- 100 ; probs <- c("0" = 0.4, "1" = 0.1)
M <- matrix(sample(0:1, rows * cols,  TRUE, probs), nrow = rows, ncol = cols)
y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE, prob = c(0.3,0.7)))
colnames(M) <- paste0("V", seq_len(ncol(M)))
laplace <- 0.5

### Train the Bernoulli Naive Bayes
bnb <- bernoulli_naive_bayes(x = M, y = y, laplace = laplace)
head(predict(bnb, newdata = M, type = "prob"))


###  Equivalent calculation with general naive_bayes function.
###  (it is made sure that the columns are factors with the 0-1 levels)

df <- as.data.frame(lapply(as.data.frame(M), factor, levels = c(0,1)))
# sapply(df, class)
nb <- naive_bayes(df, y, laplace = laplace)
head(predict(nb, type = "prob"))


# Obtain probability tables
tables(bnb, which = "V1")
tables(nb, "V1")

# Visualise class conditional Bernoulli distributions
plot(bnb, which = "V1")
plot(nb, "V1")

# Check the equivalence of the class conditional distributions
all(get_cond_dist(bnb) == get_cond_dist(nb))

# Coerce the Bernoulli probability tables to a data.frame
bernoulli_tables_to_df(bnb)

}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{plot.naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{bernoulli_tables_to_df}}
}
