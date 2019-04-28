summary.naive_bayes <- function(object, ...) {
    model <- "Naive Bayes"
    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - nchar(model)) / 2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " ", model, " ",
                       ifelse(n_char %% 2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    cat("\n")
    cat(str_full, "\n", "\n")
    cat("- Call:", deparse(object$call), "\n")
    cat("- Laplace:", object$laplace, "\n")
    cat("- Classes:", nlevels(object$data$y), "\n")
    cat("- Samples:", length(object$data$y), "\n")
    cat("- Features:", length(object$tables), "\n")
    if (getNamespaceVersion("naivebayes") >= "0.9.6") {
        cond_dists <- table(factor(get_cond_dist(object),
                                   levels = c("Bernoulli", "Categorical",
                                              "Poisson", "Gaussian", "KDE")))
        cond_dists <- cond_dists[cond_dists != 0]
        cat("- Conditional distributions: \n")
        cat("    -", paste0(names(cond_dists), ": ", cond_dists, collapse = "\n    - "))
        cat("\n")
    }
    cat("- Prior probabilities: \n")
    cat("    -", paste0(names(object$prior), ": ", round(object$prior, 4), collapse = "\n    - "))
    cat("\n\n")
    cat(l, "\n")
}
