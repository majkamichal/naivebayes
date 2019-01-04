print.naive_bayes <- function(x, ...) {

    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - 11) / 2)), collapse = "")
    str_full <- paste0(str_left_right,
                       " Naive Bayes ",
                       ifelse(n_char %% 2 != 0, "=", ""),
                       str_left_right)
    cat(str_full, "\n")
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
