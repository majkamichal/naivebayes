\name{naive_bayes}
\alias{naive_bayes}
\alias{naive_bayes.default}
\alias{naive_bayes.formula}
\title{Naive Bayes Classifier}
\usage{
\method{naive_bayes}{default}(x, y, prior = NULL, laplace = 0,
  usekernel = FALSE, usepoisson = FALSE, ...)

\method{naive_bayes}{formula}(formula, data, prior = NULL, laplace = 0,
  usekernel = FALSE, usepoisson = FALSE,
  subset, na.action = stats::na.pass, ...)
}

\arguments{
\item{x}{matrix or dataframe with categorical (character/factor/logical) or metric (numeric) predictors.}

\item{y}{class vector (character/factor/logical).}

\item{formula}{an object of class \code{"formula"} (or one that can be coerced to "formula") of the form: \code{class ~ predictors} (class has to be a factor/character/logical).}

\item{data}{matrix or dataframe with categorical (character/factor/logical) or metric (numeric) predictors.}

\item{prior}{vector with prior probabilities of the classes. If unspecified, the class proportions for the training set are used. If present, the probabilities should be specified in the order of the factor levels.}

\item{laplace}{value used for Laplace smoothing (additive smoothing). Defaults to 0 (no Laplace smoothing).}

\item{usekernel}{logical; if \code{TRUE}, \code{\link[stats]{density}} is used to estimate the class conditional densities of metric predictors. This applies to vectors with class "numeric". For further details on interaction between \code{usekernel} and \code{usepoisson} parameters please see Note below.}


\item{usepoisson}{logical; if \code{TRUE}, Poisson distribution is used to estimate the class conditional PMFs of integer predictors (vectors with class "integer").}

\item{subset}{an optional vector specifying a subset of observations to be used in the fitting process.}

\item{na.action}{a function which indicates what should happen when the data contain \code{NAs}. By default (\code{\link[stats]{na.pass}}), missing values are not removed from the data and are then omited while constructing tables. Alternatively, \code{\link[stats]{na.omit}} can be used to exclude rows with at least one missing value before constructing tables.}

\item{...}{other parameters to \code{\link[stats]{density}} when \code{usekernel = TRUE} (\code{na.rm} defaults to \code{TRUE}) (for instance \code{adjust}, \code{kernel} or \code{bw}).}

}

\value{
\code{naive_bayes} returns an object of class \code{"naive_bayes"} which is a list with following components:
    \item{data}{list with two components: \code{x} (dataframe with predictors) and \code{y} (class variable).}
    \item{levels}{character vector with values of the class variable.}
    \item{laplace}{amount of Laplace smoothing (additive smoothing).}
    \item{tables}{list of tables. For each categorical predictor a table with class-conditional probabilities, for each integer predictor a table with Poisson mean (if \code{usepoisson = TRUE}) and for each metric predictor a table with a mean and standard deviation or \code{\link[stats]{density}} objects for each class. The object \code{tables} contains also an additional attribute "cond_dist" - a character vector with the names of conditional distributions assigned to each feature.}
    \item{prior}{numeric vector with prior probabilities.}
    \item{usekernel}{logical; \code{TRUE}, if the kernel density estimation was used for estimating class conditional densities of numeric variables.}
    \item{usepoisson}{logical; \code{TRUE}, if the Poisson distribution was used for estimating class conditional PMFs of non-negative integer variables.}
    \item{call}{the call that produced this object.}
}
\description{
\code{naive_bayes} is used to fit Naive Bayes model in which predictors are assumed to be independent within each class label.
}
\details{
Numeric (metric) predictors are handled by assuming that they follow Gaussian distribution, given the class label. Alternatively, kernel density estimation can be used (\code{usekernel = TRUE}) to estimate their class-conditional distributions. Also, non-negative integer predictors (variables representing counts) can be modelled with Poisson distribution (\code{usepoisson = TRUE}); for further details please see \code{Note} below. Missing values are not included into constructing tables. Logical variables are treated as categorical (binary) variables.
}

\note{
The class "numeric" contains "double" (double precision floating point numbers) and "integer". Depending on the parameters \code{usekernel} and \code{usepoisson} different class conditional distributions are applied to columns in the dataset with the class "numeric":

\itemize{

\item If \code{usekernel=FALSE} and \code{poisson=FALSE} then Gaussian distribution is applied to each "numeric" variable ("numeric"&"integer" or "numeric"&"double")

\item If \code{usekernel=TRUE} and \code{poisson=FALSE} then kernel density estimation (KDE) is applied to each "numeric" variable ("numeric"&"integer" or "numeric"&"double")

\item If \code{usekernel=FALSE} and \code{poisson=TRUE} then Gaussian distribution is applied to each "double" vector and Poisson to each "integer" vector. (Gaussian: "numeric" & "double"; Poisson: "numeric" & "integer")

\item If \code{usekernel=TRUE} and \code{poisson=TRUE} then kernel density estimation (KDE) is applied to each "double" vector and Poisson to each "integer" vector. (KDE: "numeric" & "double"; Poisson: "numeric" & "integer")

}
By default \code{usekernel=FALSE} and \code{poisson=FALSE}, thus Gaussian is applied to each numeric variable.

On the other hand, "character", "factor" and "logical" variables are assigned to the Categorical distribution with Bernoulli being its special case.

Prior the model fitting the classes of columns in the data.frame "data" can be easily checked via:

\itemize{
\item \code{sapply(data, class)}
\item \code{sapply(data, is.numeric)}
\item \code{sapply(data, is.double)}
\item \code{sapply(data, is.integer)}
}
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
###    via kernel density estimation (KDE)
nb_kde <- naive_bayes(class ~ ., train, usekernel = TRUE)
summary(nb_kde)
get_cond_dist(nb_kde)

nb_kde \%prob\% test

# Visualize class conditional densities
plot(nb_kde, "norm", arg.num = list(legend.cex = 0.9), prob = "conditional")
plot(nb_kde, "count", arg.num = list(legend.cex = 0.9), prob = "conditional")

### ?density and ?bw.nrd for further documentation

# 3.1) Change Gaussian kernel to biweight kernel
nb_kde_biweight <- naive_bayes(class ~ ., train, usekernel = TRUE,
                               kernel = "biweight")
nb_kde_biweight \%prob\% test
plot(nb_kde_biweight, c("norm", "count"),
     arg.num = list(legend.cex = 0.9), prob = "conditional")


# 3.2) Change "nrd0" (Silverman's rule of thumb) bandwidth selector
nb_kde_SJ <- naive_bayes(class ~ ., train, usekernel = TRUE,
                               bw = "SJ")
nb_kde_SJ \%prob\% test
plot(nb_kde_SJ, c("norm", "count"),
     arg.num = list(legend.cex = 0.9), prob = "conditional")


# 3.3) Adjust bandwidth
nb_kde_adjust <- naive_bayes(class ~ ., train, usekernel = TRUE,
                         adjust = 1.5)
nb_kde_adjust \%prob\% test
plot(nb_kde_adjust, c("norm", "count"),
     arg.num = list(legend.cex = 0.9), prob = "conditional")


### 4) Model non-negative integers with Poisson distribution
nb_pois <- naive_bayes(class ~ ., train, usekernel = TRUE, usepoisson = TRUE)
summary(nb_pois)
get_cond_dist(nb_pois)

# Posterior probabilities
nb_pois \%prob\% test

# Class conditional distributions
plot(nb_pois, "count", prob = "conditional")

# Marginal distributions
plot(nb_pois, "count", prob = "marginal")


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
\code{\link{predict.naive_bayes}}, \code{\link{plot.naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}, \code{\link[naivebayes]{\%class\%}}
}

