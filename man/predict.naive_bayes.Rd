\name{predict.naive_bayes}
\alias{predict.naive_bayes}
\title{Predict Method for naive_bayes Objects}
\usage{
\method{predict}{naive_bayes}(object, newdata = NULL, type = c("class","prob"),
  threshold = 0.001, eps = 0, ...)
}
\arguments{
\item{object}{object of class inheriting from \code{"naive_bayes"}.}

\item{newdata}{matrix or dataframe with categorical (character/factor/logical) or metric (numeric) predictors.}

\item{type}{if "class", new data points are classified according to the highest posterior probabilities. If "prob", the posterior probabilities for each class are returned.}

\item{threshold}{value by which zero probabilities or probabilities within the epsilon-range corresponding to metric variables are replaced (zero probabilities corresponding to categorical variables can be handled with Laplace (additive) smoothing).}

\item{eps}{value that specifies an epsilon-range to replace zero or close to zero probabilities by \code{threshold}. It applies to metric variables.}

\item{...}{not used.}
}
\value{
\code{predict.naive_bayes} returns either a factor with class labels corresponding to the maximal conditional posterior probabilities or a matrix with class label specific conditional posterior probabilities.
}
\description{
Classification based on Naive Bayes models.
}
\details{
Computes conditional posterior probabilities for each class label using the Bayes' rule under the assumption of independence of predictors. If no new data is provided, the data from the object is used. Logical variables are treated as categorical (binary) variables. Predictors with missing values are not included into the computation of posterior probabilities.
}
\examples{
### Simulate example data
n <- 100
set.seed(1)
data <- data.frame(class = sample(c("classA", "classB"), n, TRUE),
                   bern = sample(LETTERS[1:2], n, TRUE),
                   cat  = sample(letters[1:3], n, TRUE),
                   logical = sample(c(TRUE,FALSE), n, TRUE),
                   norm = rnorm(n),
                   count = rpois(n, lambda = c(5,15)))
train <- data[1:95, ]
test <- data[96:100, -1]

### Fit the model with default settings
nb <- naive_bayes(class ~ ., train)

# Classification
predict(nb, test, type = "class")
nb \%class\% test

# Posterior probabilities
predict(nb, test, type = "prob")
nb \%prob\% test


\dontrun{
vars <- 10
rows <- 1000000
y <- sample(c("a", "b"), rows, TRUE)

# Only categorical variables
X1 <- as.data.frame(matrix(sample(letters[5:9], vars * rows, TRUE),
                           ncol = vars))
nb_cat <- naive_bayes(x = X1, y = y)
nb_cat
system.time(pred2 <- predict(nb_cat, X1))
}
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{plot.naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}
}

