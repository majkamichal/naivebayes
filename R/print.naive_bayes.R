print.naive_bayes <- function (x, ...) {

    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - 11) / 2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " Naive Bayes ",
                       ifelse(n_char %% 2 != 0, "=", ""), str_left_right)
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
    cat("\n")
    cat("Tables:")

    tabs <- x$tables
    n <- length(x$tables)
    indices <- seq_len(min(5,n))
    tabs <- tabs[indices]
    print(tabs)

    if (n > 5) {
        cat("\n\n")
        cat("# ... and", n - 5, ifelse(n - 5 == 1, "more table", "more tables\n\n"))
        cat(l)
    }
    cat("\n\n")
}
