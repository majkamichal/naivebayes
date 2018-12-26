\name{NEWS}
\title{News for Package \pkg{naivebayes}}

\section{Changes in version 0.9.3}{
	\itemize{
		\item \bold{Fixed}: numerical underflow in predict.naive_bayes function when the number of features is big (bug found by William Townes)
		\item \bold{Fixed}: when all names of features in the \code{newdata} in \code{predict.naive_bayes} function do not match these defined in the naive_bayes object, then the calculation based on prior probabilities is done only for one row of \code{newdata}
		\item \bold{Improvement}: better handling (informative warnings/errors) in case of not correct inputs in 'predict.naive_bayes' function
		\item \bold{Improvement}: \code{print.naive_bayes} fits now the console width
	}
}

\section{Changes in version 0.9.2}{
	\itemize{
		\item \bold{Fixed}: when the data have two classes and they are not alphabetically ordered, the predicted classes are incorrect (bug found by Max Kuhn)
	}
}

\section{Changes in version 0.9.1}{
	\itemize{
		\item \bold{Fixed}: when the prediction data has one row, the column names get dropped (bug found by Max Kuhn)
	}
}
