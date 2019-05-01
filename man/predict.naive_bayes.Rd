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
### 1) General usage via formula interface
nb <- naive_bayes(class ~ ., train)
summary(nb)

# Classification
predict(nb, test, type = "class")
nb \%class\% test

# Posterior probabilities
predict(nb, test, type = "prob")
nb \%prob\% test

# Helper functions
tables(nb, 1)
get_cond_dist(nb)

# Note: all "numeric" (integer, double) variables are modelled
#       with Gaussian distribution by default.


### 2) General usage via matrix/data.frame and class vector
X <- train[-1]
class <- train$class
nb2 <- naive_bayes(x = X, y = class)
nb2 \%prob\% test


### 3) Model continuous variables non-parametrically
#   via kernel density estimation (KDE)
nb_kde <- naive_bayes(class ~ ., train, usekernel = TRUE)
summary(nb_kde)
get_cond_dist(nb_kde)

nb_kde \%prob\% test
plot(nb_kde, "norm", arg.num = list(legend.cex = 0.9))
plot(nb_kde, "count", arg.num = list(legend.cex = 0.9))

# ?density and ?bw.nrd for further documentation

# 3.1) Change Gaussian kernel to biweight kernel
nb_kde_biweight <- naive_bayes(class ~ ., train, usekernel = TRUE,
                               kernel = "biweight")
nb_kde_biweight \%prob\% test
plot(nb_kde_biweight, c("norm", "count"), arg.num = list(legend.cex = 0.9))

# 3.2) Change "nrd0" (Silverman's rule of thumb) bandwidth selector
nb_kde_SJ <- naive_bayes(class ~ ., train, usekernel = TRUE,
                               bw = "SJ")
nb_kde_SJ \%prob\% test
plot(nb_kde_SJ, c("norm", "count"), arg.num = list(legend.cex = 0.9))


# 3.3) Adjust bandwidth
nb_kde_adjust <- naive_bayes(class ~ ., train, usekernel = TRUE,
                         adjust = 1.5)
nb_kde_adjust \%prob\% test
plot(nb_kde_adjust, c("norm", "count"), arg.num = list(legend.cex = 0.9))


### 4) Model non-negative integers with Poisson distribution
nb_pois <- naive_bayes(class ~ ., train, usekernel = TRUE, usepoisson = TRUE)
summary(nb_pois)
get_cond_dist(nb_pois)

nb_pois \%prob\% test
plot(nb_pois, "count")

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

