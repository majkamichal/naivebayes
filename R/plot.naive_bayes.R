plot.naive_bayes <- function(x, which = NULL, ask = FALSE, legend = TRUE,
                             legend.box = FALSE, arg.num = list(),
                             arg.cat = list(), ...) {

    vars <- names(x$tables)

    if (is.character(which) && !all(which %in% vars))
        stop("At least one variable is not available")

    if (length(which) > length(vars))
        stop("Too many variables selected")

    if (!is.null(which) && !is.character(which) && !is.numeric(which))
        stop("'which' has to be either character or numeric vector")

    if (length(list(...)) > 0)
        warning("Please specify additional parameters with 'arg.num' or 'arg.cat'")

    if (is.null(which))
        which <- seq_along(vars)

    if (is.numeric(which))
        v <- vars[which]

    if (is.character(which))
        v <- vars[vars %in% which]

    opar <- graphics::par()$ask
    graphics::par(ask = ask)
    on.exit(graphics::par(ask = opar))

    for (i in v) {
        i_tab <- x$tables[[i]]
        lev <- x$levels
        if (is.numeric(x$data$x[[i]]))  {
            if (x$usekernel) {
                bws <- round(sapply(i_tab, "[[", "bw"), 3)
                leg <- paste0(lev, " (bw: ", bws, ")")
                X <- sapply(i_tab, "[[", "x")
                Y <- sapply(i_tab, "[[", "y")
            }
            if (!x$usekernel) {
                leg <- lev
                r <- range(x$data$x[[i]], na.rm = TRUE)
                X <- seq(r[1], r[2], length.out = 512)
                Y <- matrix(stats::dnorm(x = X,
                                         mean = rep(i_tab[1, ], each = length(X)),
                                         sd   = rep(i_tab[2, ], each = length(X))),
                            ncol = length(lev))
            }
            n <- names(arg.num)
            if (!("col"  %in% n)) arg.num$col <- seq_along(lev) + 1
            if (!("type" %in% n)) arg.num$type <- "l"
            if (!("lty"  %in% n)) arg.num$lty <- seq_along(lev)
            if (!("ylab" %in% n)) arg.num$ylab <- "Density"
            arg.num$xlab <- i

            params <- c(list(x = quote(X), y = quote(Y)), arg.num)
            do.call("matplot", params)
            if (legend) {
                bty = ifelse(legend.box == TRUE, TRUE, "n")
                legend("topleft", leg, col = arg.num$col, lty = arg.num$lty,
                       title = "", cex = 1, y.intersp = 0.75, bty = bty)
            }

        } else {
            if (!("main" %in% names(arg.cat))) arg.cat$main <- ""
            if (!("color" %in% names(arg.cat))) arg.cat$color <- seq_along(lev) + 1
            arg.cat$xlab <- i
            params <- c(list(x = quote(i_tab)), c(arg.cat))
            do.call("mosaicplot", params)
        }
    }
    invisible()
}
