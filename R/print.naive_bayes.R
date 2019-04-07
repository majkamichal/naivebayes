print.naive_bayes <- function (x, ...)
{
    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - 11)/2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " Naive Bayes ",
                       ifelse(n_char%%2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    cat("\n")
    cat(str_full, "\n", "\n")
    cat("Call:", "\n")
    print(x$call)
    cat("\n")
    cat(l, "\n")

    cat("\n")
    cat("A priori probabilities:", "\n")
    print(x$prior)
    cat("\n")
    cat(l)
    cat("\n")
    cat("Tables:", "\n")

    tables <- x$tables
    n <- length(x$tables)
    for (i in 1:min(5, n)) {
        ith_tab <- tables[[i]]
        ith_name <- names(tables)[i]
        if (class(ith_tab) == "array") {
            for (ith_factor in names(ith_tab)) {
                cat(l)
                cat("\n")
                cat("\n")
                cat(paste0(ith_name, "::", ith_factor, " (KDE)", "\n"))
                print(ith_tab[[ith_factor]])
            }
        } else {
            cat(l)
            cat("\n")
            cat("\n")
            if (any(rownames(ith_tab) == "lambda") & nrow(ith_tab) == 1) {
                dist <- "(Poisson)"
            }
            if (nrow(ith_tab) == 2 & all(!rownames(ith_tab) %in% c("mean", "sd"))) {
                dist <- "(Bernoulli)"
            }
            if (nrow(ith_tab) > 2) {
                dist <- "(Multinomial)"
            }
            if (nrow(ith_tab) == 2 & all(rownames(ith_tab) %in% c("mean", "sd"))) {
                dist <- "(Gaussian)"
            }
            cat(paste0("Feature: ", ith_name, " ", dist, "\n"))
            print(ith_tab)
            cat("\n")
        }
    }
    if (n > 5) {
        cat(l)
        cat("\n\n")
        cat("# ... and", n - 5, ifelse(n - 5 == 1, "more table", "more tables\n\n"))
        cat(l)
    }
    cat("\n\n")
}
