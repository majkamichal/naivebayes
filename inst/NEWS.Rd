\name{NEWS}
\title{News for Package \pkg{naivebayes}}

\section{Changes in version 0.9.6}{
	\itemize{
	    \item Improvement: Poisson distribution is now available to model class conditional probabilities of non-negative integer predictors. It is applied to all vectors with class "integer" via a new parameter \code{usepoisson = TRUE} in \code{naive_bayes} function. By default \code{naive_bayes(..., usepoisson = FALSE}) and hence it does not change the posterior probabilities obtained with the previous versions of the package. All \code{naive_bayes} objects created with previous versions are fully compatible with the 0.9.6 version.
	    \item Improvement: new function \code{get_cond_dist()} for obtaining names of class conditional distributions assigned to features
	    \item Improvement: new function \code{bernoulli_naive_bayes()} - specialised version of the \code{naive_bayes()}, where all features take on 0-1 values and each feature is modelled with the Bernoulli distribution. Smaller flexibility, higher efficiency.
	    \item Improvement: enhanced print method for better readability
	    \item Improvement: enhanced documentation - this includes a new webpage: https://majkamichal.github.io/naivebayes/
	    \item Improvement: \code{predict.naive_bayes} has new parameter \code{eps} that specifies a value of an epsilon-range to replace zero or close to zero probabilities by specified threshold. It applies to metric variables.
	}
}

\section{Changes in version 0.9.5}{
	\itemize{
	    \item Fixed: when \code{laplace} > 0 and discrete feature with >2 distinct values, the probabilities in the probability table do not sum up to 1
	}
}


\section{Changes in version 0.9.4}{
	\itemize{
	    \item Fixed: plot crashes when missing data present in training set (bug found by Mark van der Loo)
	}
}

\section{Changes in version 0.9.3}{
	\itemize{
		\item Fixed: numerical underflow in predict.naive_bayes function when the number of features is big (bug found by William Townes)
		\item Fixed: when all names of features in the \code{newdata} in \code{predict.naive_bayes} function do not match these defined in the naive_bayes object, then the calculation based on prior probabilities is done only for one row of \code{newdata}
		\item Improvement: better handling (informative warnings/errors) of not correct inputs in 'predict.naive_bayes' function
		\item Improvement: \code{print.naive_bayes} fits now the console width
	}
}

\section{Changes in version 0.9.2}{
	\itemize{
		\item Fixed: when the data have two classes and they are not alphabetically ordered, the predicted classes are incorrect (bug found by Max Kuhn)
	}
}

\section{Changes in version 0.9.1}{
	\itemize{
		\item Fixed: when the prediction data has one row, the column names get dropped (bug found by Max Kuhn)
	}
}
