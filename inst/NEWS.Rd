\name{NEWS}
\title{News for Package \pkg{naivebayes}}

\section{Changes in version 0.9.7}{
    \itemize{
    	    \item Improvement: \code{multinomial_naive_bayes()}, \code{bernoulli_naive_bayes()}, \code{poisson_naive_bayes()} and \code{gaussian_naive_bayes()} now support sparse matrices (\code{dgCMatrix} class from the \code{Matrix} Package).

            \item Improvement: updated documentation.

            \item Improvement: better informative errors.
    	}
}

\section{Changes in version 0.9.6}{

\subsection{Improvements:}{

    \itemize{
        \item Enhanced documentation - this includes a new webpage: https://majkamichal.github.io/naivebayes/
	    \item \code{naive_bayes()}: Poisson distribution is now available to model class conditional probabilities of non-negative integer predictors. It is applied to all vectors with class "integer" via a new parameter \code{usepoisson = TRUE} in \code{naive_bayes} function. By default \code{usepoisson = FALSE}. All \code{naive_bayes} objects created with previous versions are fully compatible with the 0.9.6 version.

	    \item \code{predict.naive_bayes()} has new parameter \code{eps} that specifies a value of an epsilon-range to replace zero or close to zero probabilities by specified threshold. It applies to metric variables.
	    \item \code{predict.naive_bayes()} is now more efficient and more reliable.
	    \item \code{print()} method has been enhanced for better readability.
	    \item \code{plot()} method allows now visualising class marginal and class conditional distributions for each predictor variable via new parameter \code{prob} with two possible values: "marginal" or "conditional".
}
}

\subsection{New functions:}{
\itemize{
	    \item \code{bernoulli_naive_bayes()} - specialised version of the \code{naive_bayes()}, where all features take on 0-1 values and each feature is modelled with the Bernoulli distribution.
	    \item \code{gaussian_naive_bayes()} - specialised version of the \code{naive_bayes()}, where all features are real valued and each feature is modelled with the Gaussian distribution.
	    \item \code{poisson_naive_bayes()} - specialised version of the \code{naive_bayes()}, where all features take are non-negative integers and each feature is modelled with the Poisson distribution.
	    \item \code{nonparametric_naive_bayes()} - specialised version of the \code{naive_bayes()}, where all features take real valued and distribution of each is estimated with kernel density estimation (KDE).
	    \item \code{multinomial_naive_bayes()} - specialised Naive Bayes classifier suitable for text classification.

	    \item \%class\% and \%prob\% - infix operators that are shorthands for performing classification and obtaining posterior probabilities, respectively.

	    \item \code{coef()} - a generic function which extracts model coefficients from specialized Naive Bayes objects.

	    \item \code{get_cond_dist()} - for obtaining names of class conditional distributions assigned to features.
}
}

}


\section{Changes in version 0.9.5}{
	\itemize{
	    \item Fixed: when \code{laplace} > 0 and discrete feature with >2 distinct values, the probabilities in the probability table do not sum up to 1.
	}
}

\section{Changes in version 0.9.4}{
	\itemize{
	    \item Fixed: plot crashes when missing data present in training set (bug found by Mark van der Loo).
	}
}

\section{Changes in version 0.9.3}{
	\itemize{
		\item Fixed: numerical underflow in predict.naive_bayes function when the number of features is big (bug found by William Townes).
		\item Fixed: when all names of features in the \code{newdata} in \code{predict.naive_bayes} function do not match these defined in the naive_bayes object, then the calculation based on prior probabilities is done only for one row of \code{newdata}.
		\item Improvement: better handling (informative warnings/errors) of not correct inputs in 'predict.naive_bayes' function.
		\item Improvement: \code{print.naive_bayes} fits now the console width.
	}
}

\section{Changes in version 0.9.2}{
	\itemize{
		\item Fixed: when the data have two classes and they are not alphabetically ordered, the predicted classes are incorrect (bug found by Max Kuhn).
	}
}

\section{Changes in version 0.9.1}{
	\itemize{
		\item Fixed: when the prediction data has one row, the column names get dropped (bug found by Max Kuhn).
	}
}
