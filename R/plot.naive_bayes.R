plot.naive_bayes <- function(x, which = NULL, ask = FALSE, legend = TRUE,
                             legend.box = FALSE, arg.num = list(),
                             arg.cat = list(), prob = c("marginal", "conditional"), ...) {
    prob <- match.arg(prob)
    vars <- names(x$tables)

    if (is.null(x$data))
        stop("plot(): The \"naive_bayes\" object does not contain data", call. = FALSE)

    if (is.character(which) && !all(which %in% vars))
        stop("plot(): At least one variable is not available", call. = FALSE)

    if (length(which) > length(vars))
        stop("plot(): Too many variables selected", call. = FALSE)

    if (!is.null(which) && !is.character(which) && !is.numeric(which))
        stop("plot(): \"which\" has to be either character or numeric vector", call. = FALSE)

    if (length(list(...)) > 0)
        warning("plot(): Please specify additional parameters with 'arg.num' or 'arg.cat'", call. = FALSE)

    if (is.null(which))
        which <- seq_along(vars)

    if (is.numeric(which))
        v <- vars[which]

    if (is.character(which))
        v <- vars[vars %in% which]

    usepoisson <- ifelse(is.null(x$usepoisson), FALSE, x$usepoisson)

    opar <- graphics::par()$ask
    graphics::par(ask = ask)
    on.exit(graphics::par(ask = opar))
    prior <- x$prior
    for (i in v) {
        i_tab <- x$tables[[i]]
        lev <- x$levels
        arg.num2 <- arg.num
        if (is.numeric(x$data$x[[i]]))  {

            if (is.integer(x$data$x[[i]]) & usepoisson)  {
                leg <- lev
                X <- 0:max(x$data$x[[i]], na.rm = TRUE)
                Y <- matrix(stats::dpois(x = X, lambda = rep(i_tab, each = length(X))),
                            ncol = length(lev))
                if (prob == "marginal") {
                    for (ith_class in 1:length(prior))
                        Y[ ,ith_class] <- Y[ ,ith_class] * prior[ith_class]
                }
                n <- names(arg.num2)
                if (!("col"  %in% n)) arg.num2$col <- seq_along(lev) + 1
                if (!("las"  %in% n)) arg.num2$las <- 1
                if (("type" %in% n) ) arg.num2$type <- "o"
                if (!("type" %in% n)) arg.num2$type <- "o"
                if (("pch" %in% n)) arg.num2$pch <- 16
                if (!("pch" %in% n)) arg.num2$pch <- 16
                if (!("ylab" %in% n)) arg.num2$ylab <- "PMF"
                if (!("lab" %in% n)) arg.num2$lab <- c(10,10,2)
                if (!("cex" %in% n)) arg.num2$cex.axis <- 0.8
                if (!("cex.axis" %in% n)) arg.num2$cex.axis <- 0.75
                if (!("lty"  %in% n)) arg.num2$lty <- seq_along(lev)
                if (!("legend.position" %in% n)) arg.num2$legend.position  <- "topright"
                if (!("legend.cex" %in% n)) arg.num2$legend.cex  <- 0.85
                legend_position <- arg.num2$legend.position
                legend_cex <- arg.num2$legend.cex
                arg.num2$legend.position <- NULL
                arg.num2$legend.cex <- NULL
                arg.num2$xlab <- i
                params <- c(list(x = quote(X), y = quote(Y)), arg.num2)
                do.call("matplot", params)
                if (legend) {
                    bty = ifelse(legend.box == TRUE, TRUE, "n")
                    legend(legend_position, leg, col = arg.num2$col, lty = arg.num2$lty,
                           title = "", cex = legend_cex, y.intersp = 0.7, bty = bty,
                           pch = arg.num2$pch)
                }
            } else {
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
                if (prob == "marginal") {
                    for (ith_class in 1:length(prior))
                        Y[ ,ith_class] <- Y[ ,ith_class] * prior[ith_class]
                }
                n <- names(arg.num2)
                if (!("col"  %in% n)) arg.num2$col <- seq_along(lev) + 1
                if (!("las"  %in% n)) arg.num2$las <- 1
                if (!("type" %in% n)) arg.num2$type <- "l"
                if (!("lty"  %in% n)) arg.num2$lty <- seq_along(lev)
                if (!("ylab" %in% n)) arg.num2$ylab <- "Density"
                if (!("lab" %in% n)) arg.num2$lab <- c(10,10,2)
                if (!("cex.axis" %in% n)) arg.num2$cex.axis <- 0.75
                if (!("legend.position" %in% n)) arg.num2$legend.position  <- "topright"
                if (!("legend.cex" %in% n)) arg.num2$legend.cex  <- 0.85
                legend_position <- arg.num2$legend.position
                legend_cex <- arg.num2$legend.cex
                arg.num2$legend.position <- NULL
                arg.num2$legend.cex <- NULL
                arg.num2$xlab <- i

                params <- c(list(x = quote(X), y = quote(Y)), arg.num2)
                do.call("matplot", params)
                if (legend) {
                    bty = ifelse(legend.box == TRUE, TRUE, "n")
                    legend(legend_position, leg, col = arg.num2$col, lty = arg.num2$lty,
                           title = "", cex = legend_cex, y.intersp = 0.75, bty = bty)
                }
            }

        } else {
            if (!("main" %in% names(arg.cat))) arg.cat$main <- ""
            if (!("color" %in% names(arg.cat))) arg.cat$color <- grDevices::heat.colors(length(lev))
            arg.cat$ylab <- i
            if (prob == "marginal") {
                for (ith_class in 1:length(prior))
                    i_tab[ ,ith_class] <- i_tab[ ,ith_class] * prior[ith_class]
            }
            params <- c(list(x = quote(t(i_tab))), c(arg.cat))
            do.call("mosaicplot", params)
        }
    }
    invisible()
}
