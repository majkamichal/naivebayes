# naivebayes 0.9.7

* Improvement: `multinomial_naive_bayes()`, `bernoullie_naive_bayes()`, `poisson_naive_bayes()` and `gaussian_naive_bayes()` now support sparse matrices (`dgCMatrix` class from the `Matrix` Package).

* Improvement: updated documentation.

* Improvement: better informative errors.


# naivebayes 0.9.6

## Improvements:

* Enhanced documentation - this includes a new webpage: https://majkamichal.github.io/naivebayes/

* In `naive_bayes()` Poisson distribution is now available to model class conditional probabilities of non-negative integer predictors. It is applied to all vectors with class "integer" via a new parameter `usepoisson = TRUE`. By default `usepoisson = FALSE`. All `naive_bayes` objects created with previous versions are fully compatible with the `0.9.6` version.

* `predict.naive_bayes()` has a new parameter `eps` that specifies a value of an epsilon-range to replace zero or close to zero probabilities by specified threshold. It applies to metric variables as well as to discrete variables, but only when `laplace = 0`.

* `predict.naive_bayes()` is now more efficient and reliable.

* `print()` method has been enhanced for better readability.

* ` plot()` method allows now visualising class marginal and class conditional distributions for each predictor variable via new parameter `prob` with two possible values: `"marginal"` or `"conditional"`.

## New functions

* `bernoulli_naive_bayes()` - specialised version of `naive_bayes()`, where all features take on 0-1 values and each feature is modelled with the Bernoulli distribution.
	    
* `gaussian_naive_bayes()` - specialised version of `naive_bayes()`, where all features are real valued and each feature is modelled with the Gaussian distribution.
	   
* `poisson_naive_bayes()` - specialised version of `naive_bayes()`, where all features are non-negative integers and each feature is modelled with the Poisson distribution.

* `nonparametric_naive_bayes()` - specialised version of `naive_bayes()`, where all features are real valued and distribution of each is estimated with kernel density estimation (KDE).

* `multinomial_naive_bayes()` - specialised Naive Bayes classifier suitable for text classification.
	    
* `%class%` and `%prob%` - infix operators that are shorthands for performing classification and obtaining posterior probabilities, respectively.
	    
* `coef()` - a generic function which extracts model coefficients from specialized Naive Bayes objects.
	    
* `get_cond_dist()` - for obtaining names of class conditional distributions assigned to features.

# naivebayes 0.9.5

* Fixed: when `laplace > 0` and discrete feature with `>2` distinct values, the probabilities in the probability table do not sum up to 1.

# naivebayes 0.9.4

* Fixed: `plot.naive_bayes()` crashes when missing data present in training set (bug found by Mark van der Loo).

# naivebayes 0.9.3

* Fixed: numerical underflow in predict.naive_bayes function when the number of features is big (bug found by William Townes).

* Fixed: When all names of features in the `newdata` in `predict.naive_bayes()` do not match these defined in the `naive_bayes` object, then the calculation based on prior probabilities is done only for one row of `newdata`.

* Improvement: Better handling (informative warnings/errors) of not correct inputs in `predict.naive_bayes()`.

* Improvement: `print.naive_bayes()` is now more transparent.

# naivebayes 0.9.2

* Fixed: when the data have two classes and they are not alphabetically ordered, the predicted classes are incorrect (bug found by Max Kuhn).

# naivebayes 0.9.1

* Fixed: when the prediction data has one row, the column names get dropped (bug found by Max Kuhn).
	
