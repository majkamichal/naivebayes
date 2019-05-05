# VALIDATION: Multinomial ======================================================

# GOAL: Check if the results are correct and consistent with other implementations

# Benchmarking models:
# quanteda::textmodel_nb

library(quanteda)
library(quanteda.corpora) # devtools::install_github("quanteda/quanteda.corpora")
library(caret)
library(naivebayes)
library(rbenchmark)


# CONTENT: =====================================================================
#
# 0) Data
# 1) Benchmarking multinomial_naive_bayes
# multinomial_naive_bayes vs quanteda::textmodel_nb
#      1.1.1) Compared for laplace = 1
#               * parameter estimates
#               * posterior probabilities
#      1.1.2) NAs in training data or in test data
#               * parameter estimates
#               * posterior probabilities
#      1.1.3) Compare efficiency

# 0) DATA: =====================================================================

# Found in stackoverflow thread:
# https://stackoverflow.com/questions/54427001/naive-bayes-in-quanteda-vs-caret-wildly-different-results/54431055#54431055

corp <- data_corpus_movies
set.seed(300)
id_train <- sample(docnames(corp), size = 150, replace = FALSE)

# get training set
training_dfm <- corpus_subset(corp, docnames(corp) %in% id_train) %>%
    dfm(stem = TRUE)

# get test set (documents not in id_train, make features equal)
test_dfm <- corpus_subset(corp, !docnames(corp) %in% id_train) %>%
    dfm(stem = TRUE) %>%
    dfm_select(pattern = training_dfm,
               selection = "keep")
ytrain <- docvars(training_dfm, "Sentiment")

# Equivalent data for multinomial_naive_bayes
M <- as.matrix(training_dfm)
test <- as.matrix(test_dfm)


# 1) Benchmarking multinomial_naive_bayes: =====================================

# 1.1.1) Fit models without NAs in data: ---------------------------------------
laplace <- 1

# Fit models
nb_quanteda <- textmodel_nb(training_dfm, ytrain, smooth = laplace, prior = "uniform")
nb_naivebayes <- multinomial_naive_bayes(M, ytrain, laplace = 1, prior = c(0.5,0.5))

# Compare parameter estimates
head(t(nb_quanteda$PwGc))
head(coef(nb_naivebayes))

sum(abs(t(nb_quanteda$PwGc) - coef(nb_naivebayes)))

# Check if probabilities sum up to 1 column-wise
colSums(coef(nb_naivebayes))

# Posterior probabilities
post_quanteda <- predict(nb_quanteda, newdata = test_dfm, type = "prob")
post_naivebayes <- nb_naivebayes %prob% test

head(post_quanteda)
head(post_naivebayes)

sum(abs(post_quanteda - post_naivebayes))


# 1.1.2) Fit models with NAs in data: ------------------------------------------

# NAs in y: --------------------------------------------------------------------
laplace <- 1

n_na_y <- 10
ytrain_na <- ytrain
ytrain_na[sample(1:length(ytrain_na), n_na_y)] <- NA

# Fit models
nb_quanteda_na <- textmodel_nb(training_dfm, ytrain_na, smooth = laplace, prior = "uniform")
nb_naivebayes_na <- multinomial_naive_bayes(M, ytrain_na, laplace = 1, prior = c(0.5,0.5))

# Compare parameter estimates
head(t(nb_quanteda_na$PwGc))
head(coef(nb_naivebayes_na))

sum(abs(t(nb_quanteda_na$PwGc) - coef(nb_naivebayes_na)))

# Check if probabilities sum up to 1 column-wise
colSums(coef(nb_naivebayes_na))

# Posterior probabilities
post_quanteda_na <- predict(nb_quanteda_na, newdata = test_dfm, type = "prob")
post_naivebayes_na <- nb_naivebayes_na %prob% test

head(post_quanteda_na)
head(post_naivebayes_na)

sum(abs(post_quanteda_na - post_naivebayes_na))


# NAs in x and y: --------------------------------------------------------------
laplace <- 1

n_na_y <- 10
n_na_x <- 1000
ytrain_na2 <- ytrain
ytrain_na2[sample(1:length(ytrain_na2), n_na_y)] <- NA

training_dfm_na <- training_dfm
training_dfm_na[sample(1:length(training_dfm_na), n_na_x)] <- NA
training_dfm_na <- as.dfm(training_dfm_na)

M_na <- as.matrix(training_dfm_na)

# Fit models
nb_quanteda_na2 <- textmodel_nb(training_dfm_na, ytrain_na, smooth = laplace, prior = "uniform")
nb_naivebayes_na2 <- multinomial_naive_bayes(M_na, ytrain_na, laplace = 1, prior = c(0.5,0.5))

# Quanteda does not support NAs in x

# Check estimates
head(coef(nb_naivebayes_na2))

# Check if probabilities sum up to 1 column-wise
colSums(coef(nb_naivebayes_na2))

# Posterior probabilities
post_naivebayes_na2 <- nb_naivebayes_na2 %prob% test
head(post_naivebayes_na2)


# 1.1.3) Speed benchmark: ------------------------------------------------------

benchmark(
    quanteda = {
        nb_quanteda <- textmodel_nb(training_dfm, ytrain, smooth = 1)
        predicted_class <- predict(nb_quanteda, newdata = test_dfm)
    },
    naivebayes = {
        nb_naivebayes <- multinomial_naive_bayes(M, ytrain, laplace = 1)
        pred <- predict(nb_naivebayes, newdata = test)
    },
    replications = 10
)

