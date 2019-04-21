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

\item{...}{other parameters to \code{\link[stats]{density}} when \code{usekernel = TRUE} (\code{na.rm} defaults to \code{TRUE}).}

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
data(iris)
nb <- naive_bayes(Species ~ ., data = iris)
plot(nb)

nb_kernel <- naive_bayes(x = iris[-5], y = iris[ ,5], usekernel = TRUE)
plot(nb_kernel, which = 1) # Gaussian kernel by default (see ?density)

nb_kernel_adj2 <- naive_bayes(x = iris[-5], y = iris[ ,5], usekernel = TRUE, adjust = 2)
plot(nb_kernel_adj2, which = 1)

nb_kernel_triangular <- naive_bayes(x = iris[-5], y = iris[ ,5],
                                    usekernel = TRUE, kernel = "biweight")
plot(nb_kernel_triangular,
     which = "Sepal.Length",
     arg.num = list(main = c("Biweight kernel")))



\dontrun{
vars <- 10
rows <- 500000
y <- sample(letters[1:2], rows, TRUE)

# Only categorical variables
X1 <- as.data.frame(matrix(sample(letters[5:9], vars * rows, TRUE),
                           ncol = vars))
format(object.size(X1), "Mb")
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
\code{\link{predict.naive_bayes}}, \code{\link{plot.naive_bayes}}, \code{\link[naivebayes]{tables}}, \code{\link[naivebayes]{get_cond_dist}}
}

