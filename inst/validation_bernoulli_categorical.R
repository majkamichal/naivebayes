
# VALIDATION: Bernoulli and Categorical  =======================================
#
# Benchmarking models:
# - e1071 and klaR for Bernoulli and categorical
#
library(e1071)
library(klaR)
#
# naive_bayes vs bernoulli_naive_bayes vs e1071::naiveBayes vs klaR::NaiveBayes
#
#
# CONTENT: =====================================================================
#
# 0) Simulated data
# 1) naive_bayes
# 1.1) Bernoulli
# naive_bayes vs bernoulli_naive_bayes vs e1071::naiveBayes vs klaR::NaiveBayes
#      1.1.1) Compared for laplace = 0, laplace > 0 and different priors
#               * parameter estimates
#               * posterior probabilities
#      1.1.2) NAs in training data or in test data
#               * parameter estimates
#               * posterior probabilities
#      1.1.3) Special cases
# 1.2) Categorical
#      1.2.1) Special cases
#             (Everything works the same way, thus, only special cases are covered)

# DATA for naive_bayes: ========================================================
n <- 100
n_test <- 5
set.seed(3)
data <- data.frame(class = sample(c("classA", "classB"), n, TRUE),
                   bern = sample(LETTERS[1:2], n, TRUE),
                   bern2 = sample(LETTERS[3:4], n, TRUE),
                   cat  = sample(letters[1:3], n, TRUE),
                   cat2  = sample(letters[4:7], n, TRUE),
                   norm = rnorm(n),
                   norm2 = rnorm(n, 100, 15),
                   count = rpois(n, lambda = 5),
                   count2 = rpois(n, lambda = 50))
ind_train <- 1:(n-n_test)
ind_test <- ((n-n_test)+1):n
train <- data[ind_train, ]
test <- data[ind_test, -1]

test_bern <- test[ c("bern", "bern2")]

# EQUIVALENT DATA for bernoulli_naive_bayes: ===================================
ytrain <- train$class
X_bern <- sapply(data[ ,c("bern", "bern2")], function(x) as.numeric(x) - 1)
X_bern_train <- X_bern[ind_train, ,drop = FALSE]
X_bern_test <- X_bern[ind_test, ,drop = FALSE]


# 1) naive_bayes: ==============================================================

# 1.1) Bernoulli: ==============================================================
#
# naive_bayes vs bernoulli_naive_bayes vs e1071::naiveBayes
#
# Overview: models are fitted based on two simulated factors each with two levels.
#
# 1.1.1) =======================================================================

# 0 = Additive smoothing: ======================================================

nb_bern <- naive_bayes(class ~ bern + bern2, train)
bnb <- bernoulli_naive_bayes(x = X_bern_train, y = ytrain)
e10_bern <- e1071::naiveBayes(class ~ bern + bern2, train)

# Compare parameter estimates for bern
tables(nb_bern, 1)
tables(bnb, 1)
t(e10_bern$tables$bern)

# Check for differences
sum(abs(tables(nb_bern, 1)[[1]] - tables(nb_bern, 1)[[1]]))
sum(abs(tables(nb_bern, 1)[[1]] - t(e10_bern$tables$bern)))

# Check if probabilities sum up to 1 column-wise
colSums(tables(nb_bern, 1)[[1]])
colSums(tables(bnb, 1)[[1]])
colSums(t(e10_bern$tables$bern))

# Compare parameter estimates for bern2
tables(nb_bern, 2)
tables(bnb, 2)
t(e10_bern$tables$bern2)

# Check for differences
sum(abs(tables(nb_bern, 2)[[1]] - tables(nb_bern, 2)[[1]]))
sum(abs(tables(nb_bern, 2)[[1]] - t(e10_bern$tables$bern2)))

# Check if probabilities sum up to 1 column-wise
colSums(tables(nb_bern, 2)[[1]])
colSums(tables(bnb, 2)[[1]])
colSums(t(e10_bern$tables$bern2))

# Check posterior probabilities
pred_nb_bern <- predict(nb_bern, newdata = test_bern, type = "prob")
pred_nb_bern2 <- nb_bern %prob% test_bern
pred_bnb <- predict(bnb, newdata = X_bern_test, type = "prob")
pred_e10_bern <- predict(e10_bern, newdata = test_bern, type = "raw")

pred_nb_bern
pred_nb_bern2
pred_bnb
pred_e10_bern

# Check for absolute differences
sum(abs(pred_nb_bern - pred_nb_bern2))
sum(abs(pred_nb_bern - pred_bnb))
sum(abs(pred_nb_bern - pred_e10_bern))




# 0 < Additive smoothing: ======================================================

smooth <- 0.5

nb_bern_smoothed <- naive_bayes(class ~ bern + bern2, train, laplace = smooth)
bnb_smoothed <- bernoulli_naive_bayes(x = X_bern_train, y = ytrain, laplace = smooth)
e10_bern_smoothed <- e1071::naiveBayes(class ~ bern + bern2, train, laplace = smooth)

# Compare parameter estimates for bern
tables(nb_bern_smoothed, 1)
tables(bnb_smoothed, 1)
t(e10_bern_smoothed$tables$bern)

# Check for differences
sum(abs(tables(nb_bern_smoothed, 1)[[1]] - tables(nb_bern_smoothed, 1)[[1]]))
sum(abs(tables(nb_bern_smoothed, 1)[[1]] - t(e10_bern_smoothed$tables$bern)))

# Check if probabilities sum up to 1 column-wise
colSums(tables(nb_bern_smoothed, 1)[[1]])
colSums(tables(bnb_smoothed, 1)[[1]])
colSums(t(e10_bern_smoothed$tables$bern))

# Compare parameter estimates for bern2
tables(nb_bern_smoothed, 2)
tables(bnb_smoothed, 2)
t(e10_bern_smoothed$tables$bern2)

# Check for differences
sum(abs(tables(nb_bern_smoothed, 2)[[1]] - tables(nb_bern_smoothed, 2)[[1]]))
sum(abs(tables(nb_bern_smoothed, 2)[[1]] - t(e10_bern_smoothed$tables$bern2)))

# Check if probabilities sum up to 1 column-wise
colSums(tables(nb_bern_smoothed, 2)[[1]])
colSums(tables(bnb_smoothed, 2)[[1]])
colSums(t(e10_bern_smoothed$tables$bern2))

# Check posterior probabilities
pred_nb_bern_smoothed <- predict(nb_bern_smoothed, newdata = test_bern, type = "prob")
pred_nb_bern2_smoothed <- nb_bern_smoothed %prob% test_bern
pred_bnb_smoothed <- predict(bnb_smoothed, newdata = X_bern_test, type = "prob")
pred_e10_bern_smoothed <- predict(e10_bern_smoothed, newdata = test_bern, type = "raw")

pred_nb_bern_smoothed
pred_nb_bern2_smoothed
pred_bnb_smoothed
pred_e10_bern_smoothed

# Check for absolute differences
sum(abs(pred_nb_bern_smoothed - pred_nb_bern2_smoothed))
sum(abs(pred_nb_bern_smoothed - pred_bnb_smoothed))
sum(abs(pred_nb_bern_smoothed - pred_e10_bern_smoothed))




# Custom priors: ===============================================================

# c(0,1) prior: ----------------------------------------------------------------
# Fix in 0.9.7 NaN when one class has prior probability == 0

prior1 <- c(0, 1)
nb_bern_prior1 <- naive_bayes(class ~ bern + bern2, train, prior = prior1)
bnb_prior1 <- bernoulli_naive_bayes(x = X_bern_train, y = ytrain, prior = prior1)
klar_bern_prior1 <- klaR::NaiveBayes(class ~ bern + bern2, train, prior = prior1)


# Check posterior probabilities
pred_nb_bern_prior1 <- predict(nb_bern_prior1, newdata = test_bern, type = "prob")
pred_nb_bern2_prior1 <- nb_bern_prior1 %prob% test_bern
pred_bnb_prior1 <- predict(bnb_prior1, newdata = X_bern_test, type = "prob")
pred_klar_bern_prior1 <- predict(klar_bern_prior1, newdata = test_bern)$posterior

pred_nb_bern_prior1
pred_nb_bern2_prior1
pred_bnb_prior1
pred_klar_bern_prior1


# c(0.0000001, 1-0.0000001) prior: ----------------------------------------------------------------

prior2 <- c(0.0000001, 1-0.0000001)
nb_bern_prior2 <- naive_bayes(class ~ bern + bern2, train, prior = prior2)
bnb_prior2 <- bernoulli_naive_bayes(x = X_bern_train, y = ytrain, prior = prior2)
klar_bern_prior2 <- klaR::NaiveBayes(class ~ bern + bern2, train, prior = prior2)

# Check posterior probabilities
pred_nb_bern_prior2 <- predict(nb_bern_prior2, newdata = test_bern, type = "prob")
pred_nb_bern2_prior2 <- nb_bern_prior2 %prob% test_bern
pred_bnb_prior2 <- predict(bnb_prior2, newdata = X_bern_test, type = "prob")
pred_klar_bern_prior2 <- predict(klar_bern_prior2, newdata = test_bern)$posterior

pred_nb_bern_prior2
pred_nb_bern2_prior2
pred_bnb_prior2
pred_klar_bern_prior2

# Check for absolute differences
sum(abs(pred_nb_bern_prior2 - pred_nb_bern2_prior2))
sum(abs(pred_nb_bern_prior2 - pred_bnb_prior2))
sum(abs(pred_nb_bern_prior2 - pred_klar_bern_prior2))



# 1.1.2) NAs ===================================================================

# Missing values in train data: ------------------------------------------------

train_na <- train[,c("class", "bern", "bern2")]

# Add NAs at random
N_na1 <- 5
N_na2 <- 5
N_nay <- 5

train_na[sample(1:length(train_na$bern), N_na1), "bern"] <- NA
train_na[sample(1:length(train_na$bern), N_na2), "bern2"] <- NA
train_na[sample(1:length(train_na$bern), N_na2), "class"] <- NA

ytrain_na <- train_na$class
X_bern_train_na <- sapply(train_na[ ,c("bern", "bern2")], function(x) as.numeric(x) - 1)


nb_bern_na <- naive_bayes(class ~ bern + bern2, train_na)
bnb_na <- bernoulli_naive_bayes(x = X_bern_train_na, y = ytrain_na)
e10_bern_na <- e1071::naiveBayes(class ~ bern + bern2, train_na)

# Compare parameter estimates for bern
tables(nb_bern_na, 1)
tables(bnb_na, 1)
t(e10_bern_na$tables$bern)

# Check for differences
sum(abs(tables(nb_bern_na, 1)[[1]] - tables(nb_bern_na, 1)[[1]]))
sum(abs(tables(nb_bern_na, 1)[[1]] - t(e10_bern_na$tables$bern)))

# Check if probabilities sum up to 1 column-wise
colSums(tables(nb_bern_na, 1)[[1]])
colSums(tables(bnb_na, 1)[[1]])
colSums(t(e10_bern_na$tables$bern))

# Compare parameter estimates for bern2
tables(nb_bern_na, 2)
tables(bnb_na, 2)
t(e10_bern_na$tables$bern2)

# Check for differences
sum(abs(tables(nb_bern_na, 2)[[1]] - tables(nb_bern_na, 2)[[1]]))
sum(abs(tables(nb_bern_na, 2)[[1]] - t(e10_bern_na$tables$bern2)))

# Check if probabilities sum up to 1 column-wise
colSums(tables(nb_bern_na, 2)[[1]])
colSums(tables(bnb_na, 2)[[1]])
colSums(t(e10_bern_na$tables$bern2))

# Check posterior probabilities
pred_nb_bern_na <- predict(nb_bern_na, newdata = test_bern, type = "prob")
pred_nb_bern2_na <- nb_bern_na %prob% test_bern
pred_bnb_na <- predict(bnb_na, newdata = X_bern_test, type = "prob")
pred_e10_bern_na <- predict(e10_bern_na, newdata = test_bern, type = "raw")

pred_nb_bern_na
pred_nb_bern2_na
pred_bnb_na
pred_e10_bern_na

# Check for absolute differences
sum(abs(pred_nb_bern_na - pred_nb_bern2_na))
sum(abs(pred_nb_bern_na - pred_bnb_na))
sum(abs(pred_nb_bern_na - pred_e10_bern_na))


# Missing values in test data: -------------------------------------------------

test_bern_na <- test_bern
X_bern_test_na <- X_bern_test

# Add two missing values
test_bern_na[1,"bern"] <- NA
X_bern_test_na[1,"bern"] <- NA

test_bern_na[4,"bern2"] <- NA
X_bern_test_na[4,"bern2"] <- NA

# Check posterior probabilities
pred_nb_bern_na_test <- predict(nb_bern_na, newdata = test_bern_na, type = "prob")
pred_nb_bern2_na_test <- nb_bern_na %prob% test_bern_na
pred_bnb_na_test <- predict(bnb_na, newdata = X_bern_test_na, type = "prob")
pred_e10_bern_na_test <- predict(e10_bern_na, newdata = test_bern_na, type = "raw")

pred_nb_bern_na_test
pred_nb_bern2_na_test
pred_bnb_na_test
pred_e10_bern_na_test

# Check for absolute differences
sum(abs(pred_nb_bern_na - pred_nb_bern2_na))
sum(abs(pred_nb_bern_na - pred_bnb_na))
sum(abs(pred_nb_bern_na - pred_e10_bern_na))



# SPECIAL CASES: ===============================================================

# - One variable and one observation in test
nb_bern <- naive_bayes(class ~ bern + bern2, train)
nb_bern %prob% test[1,1, drop = FALSE]

bnb <- bernoulli_naive_bayes(x = X_bern_train, y = ytrain)
bnb %prob% X_bern_test[1,1, drop = FALSE]

e10_bern <- e1071::naiveBayes(class ~ bern + bern2, train)
predict(e10_bern, newdata = test[1,1, drop = FALSE], type = "raw")




# 1.2) Categorical: ============================================================

train_cat <- train[ ,c("class", "cat", "cat2")]
test_cat <- test[ ,c("cat", "cat2")]

train_cat[1,"class"] <- NA
train_cat[5,"cat"] <- NA
train_cat[23,"cat2"] <- NA
test_cat[1,"cat"] <- NA

# Train models
nb_cat <- naive_bayes(class ~ cat + cat2, data = train_cat)
e10_cat <- e1071::naiveBayes(class ~ cat + cat2, data = train_cat)

tables(nb_cat, 1)
t(e10_cat$tables$cat)

# Check absolute differences of parameter estimates
sum(abs(tables(nb_cat, 1)[[1]] - t(e10_cat$tables$cat)))

# Check posterior probabilities
pred_nb_cat <- nb_cat %prob% test_cat
pred_e10_cat <- predict(e10_cat, newdata = test_cat, type = "raw")

pred_nb_cat
pred_e10_cat

# Check for absolute differences
sum(abs(pred_nb_cat - pred_e10_cat))


# 1.2.1) SPECIAL CASES: --------------------------------------------------------

# Check when there are too few/many levels in predictor variable in test data

# - Too many levels
test_cat_more_levels <- test_cat
levels(test_cat_more_levels$cat) <- c(levels(test_cat_more_levels$cat), "newlevel")
test_cat_more_levels$cat[2] <- "newlevel"

nb_cat %prob% test_cat_more_levels
# Informative error is correctly given (issues with the data that have to be
# resolved by the user)


# - Too many levels
