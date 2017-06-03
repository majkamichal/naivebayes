print.naive_bayes <- function(x, ...) {
    cat("===================== Naive Bayes =====================", "\n")
    cat("Call:", "\n")
    print(x$call)
    cat("\n")
    cat("A priori probabilities:", "\n")
    print(x$prior)
    cat("\n")
    cat("Tables:", "\n")
    n <- length(x$tables)
    for (i in 1:n) {
        if (i >= 6) next
        print(x$tables[[i]])
        cat("\n")
    }
    if (n > 5) cat("# ... and", n - 5, "more tables")
}