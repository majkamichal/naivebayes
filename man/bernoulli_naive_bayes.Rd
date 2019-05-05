\name{bernoulli_naive_bayes}
\alias{bernoulli_naive_bayes}

\title{Bernoulli Naive Bayes Classifier}
\usage{
bernoulli_naive_bayes(x, y, prior = NULL, laplace = 0, ...)
}
\arguments{
\item{x}{matrix with numeric 0-1 predictors.}

\item{y}{class vector (character/factor/logical).}

\item{prior}{vector with prior probabilities of the classes. If unspecified, the class proportions for the training set are used. If present, the probabilities should be specified in the order of the factor levels.}

\item{laplace}{value used for Laplace smoothing (additive smoothing). Defaults to 0 (no Laplace smoothing).}

\item{...}{not used.}

}

\value{
\code{bernoulli_naive_bayes} returns an object of class "bernoulli_naive_bayes" which is a list with following components:
    \item{data}{list with two components: \code{x} (matrix with predictors) and \code{y} (class variable).}
    \item{levels}{character vector with values of the class variable.}
    \item{laplace}{amount of Laplace smoothing (additive smoothing).}
    \item{prob1}{matrix with class conditional probabilities for the value 1. Based on this matrix full probability tables can be constructed. Please, see \code{\link[naivebayes]{tables}} and \code{\link[naivebayes]{coef}}.}
    \item{prior}{numeric vector with prior probabilities.}
    \item{call}{the call that produced this object.}
}
\description{
\code{bernoulli_naive_bayes} is used to fit the Bernoulli Naive Bayes model in which all class conditional distributions are assumed to be Bernoulli and be independent.
}

\details{

This is a specialized version of the Naive Bayes classifier, in which all features take on numeric 0-1 values and class conditional probabilities are modelled with the Bernoulli distribution.

The Bernoulli Naive Bayes is available in both, \code{naive_bayes} and \code{bernoulli_naive_bayes}. The implementation of the specialized Naive Bayes provides more efficient performance though. The speedup comes from the restricting the data input to a numeric 0-1 matrix and performing the linear algebra as well as vectorized operations on it. In other words, the efficiency comes at cost of the flexibility.

The \code{bernoulli_naive_bayes} function is equivalent to the \code{naive_bayes} function when the numeric 0-1 matrix is coerced to, for instance, a 0-1 character matrix.

The missing values (NAs) are omited while constructing the probability tables. The NAs in the newdata in \code{predict.gaussian_naive_bayes} are not included into the calculation of posterior probabilities; and if present an informative warning is given.
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
bnb
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
coef(bnb)

# Visualise class conditional Bernoulli distributions
plot(bnb, which = "V1", prob = "conditional")
plot(nb, "V1")

# Check the equivalence of the class conditional distributions
all(get_cond_dist(bnb) == get_cond_dist(nb))
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{predict.bernoulli_naive_bayes}}, \code{\link{plot.bernoulli_naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}
}

