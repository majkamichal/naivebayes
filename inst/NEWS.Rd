\name{NEWS}
\title{News for Package \pkg{naivebayes}}

\section{Changes in version 0.9.3}{
	\itemize{
		\item Fixed: numerical underflow in predict.naive_bayes function when the number of features is big (bug found by William Townes)
		\item Fixed: when all names of features in the 'newdata' do not match these defined in the naive_bayes object, then the calculation based on prior probabilities is done only for one row of 'newdata'
		\item Improvement: better handling (informative warnings/errors) of not correct inputs in predict.naive_bayes function?
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
