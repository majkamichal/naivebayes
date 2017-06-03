\name{predict.naive_bayes}
\alias{predict.naive_bayes}
\title{Predict Method for naive_bayes Objects}
\usage{
\method{predict}{naive_bayes}(object, newdata = NULL, type = c("class","prob"),
  threshold = 0.001, ...)
}
\arguments{
\item{object}{object of class inheriting from \code{"naive_bayes"}.}

\item{newdata}{numeric matrix or dataframe with categorical (character/factor/logical) or metric (numeric) predictors.}

\item{type}{if "class", new data points are classified according to the highest posterior probabilities. If "prob", the posterior probabilities for each class are returned.}

\item{threshold}{value by which zero probabilities corresponding to metric variables are replaced (zero probabilities corresponding to categorical variables can be handled with Laplace smoothing).}

\item{...}{not used.}
}
\value{
\code{predict.naive_bayes} returns a factor with class corresponding to the maximal conditional posterior probability or a matrix with conditional posterior probabilities for each class.
}
\description{
Classification based on Naive Bayes models.
}
\details{
Computes conditional posterior probabilities for each class using the Bayes' rule. If no new data is provided, the data from the object is used. Logical variables are treated as categorical variables. Predictors with missing values are not included into the computation of posterior probabilities.
}
\examples{
ind_iris <- sample(1:nrow(iris), size = round(0.3 * nrow(iris)))
iris_train <- iris[-ind_iris, ]
iris_test <- iris[ind_iris, ]

nb_iris <- naive_bayes(Species ~ ., iris_train)
predict(nb_iris, iris_test)
head(predict(nb_iris, iris_test, type = "prob"))


\dontrun{
vars <- 10
rows <- 500000
y <- sample(c("a", "b"), rows, TRUE)

# Only categorical variables
X1 <- as.data.frame(matrix(sample(letters[5:9], vars * rows, TRUE),
                           ncol = vars))
nb_cat <- naive_bayes(x = X1, y = y)
nb_cat
system.time(pred2 <- predict(nb_cat, X1))

# Only numeric variables
X2 <- as.data.frame(matrix(rnorm(vars * rows), ncol = vars))
nb_num <- naive_bayes(x = X2, y = y)
nb_num
system.time(pred2 <- predict(nb_num, X2))
}
}
\author{
Michal Majka, \email{michalmajka@hotmail.com}
}
\seealso{
\code{\link{naive_bayes}}, \code{\link{plot.naive_bayes}}, \code{\link[naivebayes]{tables}}
}

