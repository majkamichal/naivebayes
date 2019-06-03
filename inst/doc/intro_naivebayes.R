## ----eval=FALSE----------------------------------------------------------
#  install.packages("naivebayes")

## ----eval=FALSE----------------------------------------------------------
#  # path_to_tar.gz file <- " "
#  install.packages(path_to_tar.gz, repos = NULL, type = "source")

## ----eval=FALSE----------------------------------------------------------
#  library(naivebayes)

## ------------------------------------------------------------------------
library(naivebayes)

### Simulate data
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


### General usage via formula interface
nb <- naive_bayes(class ~ ., train, usepoisson = TRUE)
summary(nb)

# Classification
predict(nb, test, type = "class")

# Alternatively
nb %class% test

# Posterior probabilities
predict(nb, test, type = "prob")

# Alternatively
nb %prob% test

### Helper functions

# Obtain first table
tables(nb, 1)

# Get names of assigned class conditional distributions
get_cond_dist(nb)

## ------------------------------------------------------------------------
vars <- 10
rows <- 1000000
y <- sample(c("a", "b"), rows, TRUE)

# Discrete features
X1 <- as.data.frame(matrix(sample(letters[5:9], vars * rows, TRUE),
                           ncol = vars))
nb_cat <- naive_bayes(x = X1, y = y)
system.time(pred2 <- predict(nb_cat, X1))

## ----eval=FALSE----------------------------------------------------------
#  library(naivebayes)
#  
#  # Prepare data: --------------------------------------------------------
#  data(iris)
#  iris2 <- iris
#  N <- nrow(iris2)
#  n_new_factors <- 3
#  factor_names <- paste0("level", 1:n_new_factors)
#  
#  # Add a new artificial features with three levels/categories:
#  # level1 is very unlikely and has 0.5% chance to occur
#  # level2 and level3 happen with probability 75% and 29.5%, respectively
#  
#  set.seed(2)
#  iris2$new <- factor(sample(paste0("level", 1:n_new_factors),
#                             prob = c(0.005, 0.7, 0.295),
#                             size = 150,
#                             replace = TRUE), levels = factor_names)
#  
#  # Define class and feature levels: -------------------------------------
#  Ck <- "setosa"
#  level1 <- "level1"
#  level2 <- "level2"
#  level3 <- "level3"
#  
#  # level1 did not show up in the sample but we know that it
#  # has 0.5% probability to occur.
#  table(iris2$new)
#  
#  # For this reason level1 is also not available in any class sub-sample
#  table(iris2$new[iris$Species == Ck])
#  
#  # Parameter estimation: ------------------------------------------------
#  
#  # ML-estimates
#  ck_sub_sample <- table(iris2$new[iris$Species == Ck])
#  ck_mle_estim <-  ck_sub_sample / sum(ck_sub_sample)
#  
#  # Bayesian estimation via symmetric Dirichlet prior with
#  # concentration parameter 0.5.
#  # (corresponds to the Jeffreys  uninformative prior)
#  
#  laplace <- 0.5 # Jeffreys  prior / Dirichlet
#                 # with the concentration parameter 0.5
#  N1 <- sum(iris2$Species == Ck & iris2$new == level1) + laplace
#  N2 <- sum(iris2$Species == Ck & iris2$new == level2) + laplace
#  N3 <- sum(iris2$Species == Ck & iris2$new == level3) + laplace
#  N <-  sum(iris2$Species == Ck) + laplace * n_new_factors
#  ck_bayes <- c(N1, N2, N3) / N
#  
#  # Compare estimates
#  rbind(ck_mle_estim, ck_bayes)
#  
#  # Bayesian estimate for level1 has positive probability
#  # but is slightly overestimated. Compared to MLE,
#   # estimates for level2 and level3 have been slightly shrunken.
#  
#  # In general, the higher value of laplace, the more resulting
#  # distribution tends to the uniform distribution.
#  # When laplace would be set to infinity
#  # then the estimates for level1, level2 and level3
#  # would be 1/3, 1/3 and 1/3.
#  
#  # comparison with estimates obtained with naive_bayes function:
#  nb_mle <- naive_bayes(Species ~ new, data = iris2)
#  nb_bayes <- naive_bayes(Species ~ new, data = iris2,
#                          laplace = laplace)
#  
#  # MLE
#  rbind(ck_mle_estim,
#        "nb_mle" = tables(nb_mle, which = "new")[[1]][ ,Ck])
#  
#  # Bayes
#  rbind(ck_bayes,
#        "nb_bayes" = tables(nb_nb_jeffrey, which = "new")[[1]][ ,Ck])

## ----eval = FALSE--------------------------------------------------------
#  
#  data(iris)
#  Xi <- "Petal.Width" # i-th feature
#  Ck <- "versicolor"  # k-th class
#  
#  # Build class sub-sample for the i-th feature
#  Ck_Xi_subsample <- iris[iris$Species == Ck, Xi]
#  
#  # MLE
#  mle_norm <- cbind("mean" = mean(Ck_Xi_subsample),
#                    "sd" = sd(Ck_Xi_subsample))
#  
#  # MLE in naive_bayes function
#  nb_mle <- naive_bayes(x = iris[Xi], y = iris[["Species"]])
#  rbind(mle_norm,
#        "nb_mle" = tables(nb_mle, which = Xi)[[Xi]][ ,Ck])

## ----eval=FALSE----------------------------------------------------------
#  # Prepare data: --------------------------------------------------------
#  
#  data(iris)
#  Xi <- "Sepal.Width" # i-th feature
#  C1 <- "setosa"      # 1st class
#  C2 <- "virginica"   # 2nd class
#  C3 <- "versicolor"  # 3rd class
#  
#  # Build class sub-samples for the i-th feature
#  C1_Xi_subsample <- iris[iris$Species == C1, Xi]
#  C2_Xi_subsample <- iris[iris$Species == C2, Xi]
#  C3_Xi_subsample <- iris[iris$Species == C3, Xi]
#  
#  # Estimate class conditional densities for the i-th feature
#  dens1 <- density(C1_Xi_subsample)
#  dens2 <- density(C2_Xi_subsample)
#  dens3 <- density(C3_Xi_subsample)
#  
#  # Visualisation: -------------------------------------------------------
#  plot(dens2, main = "", col = "red")
#  lines(dens1, main = "", col = "blue")
#  lines(dens3, main = "", col = "black")
#  legend("topleft", legend = c(C1,C2,C3),
#         col = c("blue", "red", "black"),
#         lty = 1)
#  
#  # Compare to the naive_bayes: ------------------------------------------
#  nb_kde <- naive_bayes(x = iris[Xi], y = iris[["Species"]],
#                        usekernel = TRUE)
#  plot(nb_kde, prob = "conditional")
#  
#  dens3
#  nb_kde$tables[[Xi]][[C3]]
#  tables(nb_kde, Xi)[[1]][[C3]]
#  
#  
#  # Use custom bandwidth selector: ---------------------------------------
#  ?bw.SJ
#  nb_kde_SJ_bw <- naive_bayes(x = iris[Xi], y = iris[["Species"]],
#                        usekernel = TRUE, bw = "SJ")
#  plot(nb_kde, prob = "conditional")
#  
#  
#  # Visualize all available kernels: -------------------------------------
#  kernels <- c("gaussian", "epanechnikov", "rectangular","triangular",
#              "biweight", "cosine", "optcosine")
#  iris3 <- iris
#  iris3$one <- 1
#  
#  sapply(kernels, function (ith_kernel) {
#      nb <- naive_bayes(formula = Species ~ one, data = iris3,
#                        usekernel = TRUE, kernel = ith_kernel)
#      plot(nb, arg.num = list(main = paste0("Kernel: ", ith_kernel),
#                              col = "black"), legend = FALSE)
#      invisible()
#  })
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  # Simulate data: -------------------------------------------------------
#  cols <- 2
#  rows <- 10
#  set.seed(11)
#  M <- matrix(rpois(rows * cols, lambda = c(0.1,1)), nrow = rows,
#              ncol = cols)
#  y <- factor(sample(paste0("class", LETTERS[1:2]), rows, TRUE))
#  colnames(M) <- paste0("Var", seq_len(ncol(M)))
#  
#  Xi <- M[ ,"Var1", drop = FALSE]
#  
#  # MLE: -----------------------------------------------------------------
#  # Estimate lambdas for each class
#  tapply(Xi, y, mean)
#  
#  # Compare with naive_bayes
#  pnb <- naive_bayes(x = Xi, y = y, usepoisson = TRUE)
#  tables(pnb,1)
#  
#  # Adding pseudo-counts via laplace parameter: --------------------------
#  laplace <- 1
#  Xi_pseudo <- Xi
#  Xi_pseudo[y == "classB",][1] <- Xi_pseudo[y == "classB",][1] + laplace
#  Xi_pseudo[y == "classA",][1] <- Xi_pseudo[y == "classA",][1] + laplace
#  
#  # Estimates
#  tapply(Xi_pseudo, y, mean)
#  
#  # Compare with naive_bayes
#  pnb <- naive_bayes(x = Xi, y = y, usepoisson = TRUE, laplace = laplace)
#  tables(pnb,1)
#  

