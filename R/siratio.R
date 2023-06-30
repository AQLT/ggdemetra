#' SI-ratio
#' @param x input model or data.
#' @param labels labels.
#' @param add boolean indicating whether a new plot should be drawn.
#' @param box boolean indicating a box around the current plot should be drawn.
#' @param col.s,col.i,col.mean colors of the different components.
#' @param cex.i,lwd.s,lwd.mean graphical parameters.
#' @param xlim,ylim X and Y axis limits.
#' @param main,xlab,ylab title, X and Y axis label.
#' @param ... unused parameters.
#' 
#' @examples 
#' x <- RJDemetra::x13(ipi_c_eu[,"FR"])
#' siratioplot(x)
#' ggsiratioplot(x)
#' @importFrom stats cycle frequency
#' @importFrom graphics axis lines par plot.new plot.window points segments title
#' @rdname siratio
#' @export
siratio <- function(x, ...) {
    UseMethod("siratio", x)
}
#' @export
siratio.X13 <- function(x, ...){
    res <- x$decomposition$si_ratio
    colnames(res) <- c("si", "s")
    res
}
#' @export
siratio.TRAMO_SEATS <- function(x, ...){
    s  <- x$decomposition$components[, "s_cmp"]
    i <- x$decomposition$components[, "i_cmp"]
    mode <- x$decomposition$mode
    
    if (mode == "Additive") {
        si <- s + i
    } else {
        si <- s * i
    }
    res <- ts.union(si, s)
    colnames(res) <- c("si", "s")
    res
}
#' @export
siratio.jSA <- function(x, ...){
    res <- RJDemetra::get_indicators(x, c("decomposition.d8", "decomposition.d10"))
    
    if (is.null(res[[1]])) {
        # TRAMO-SEATS model
        res <- RJDemetra::get_indicators(x, c("decomposition.i_cmp", "decomposition.s_cmp"))
        mode <- RJDemetra::get_indicators(x, "mode")[[1]]
        if (mode == "Additive"){
            res[[1]] <- res[[1]] + res[[2]]
        } else {
            res[[2]] <- res[[1]] * res[[2]]
        }
    }
    if (is.null(res[[1]])) # neither X-13 nor TRAMO-SEATS
        return(NULL)
    res <- ts.union(res[[1]], res[[2]])
    colnames(res) <- c("si", "s")
    res
}

#' @rdname siratio
#' @export
siratioplot <- function(x, labels = NULL,
                        add = FALSE, box = TRUE,
                        col.s = "darkblue", col.i = "gray", col.mean = "red",
                        cex.i = 0.1,
                        lwd.s = par("lwd"), lwd.mean = lwd.s,
                        main = "SI ratio",
                        xlab = NULL, ylab = NULL,
                        xlim = NULL, ylim = NULL,
                        ...) {
    UseMethod("siratioplot", x)
}
#' @export
siratioplot.SA <- function(x, ...){
    siratioplot(siratio(x), ...)
}
#' @export
siratioplot.jSA <- function(x, ...){
    siratioplot(siratio(x), ...)
}
#' @export
siratioplot.default <- function(x, labels = NULL,
                                add = FALSE, box = TRUE,
                                col.s = "darkblue", col.i = "gray", col.mean = "red",
                                cex.i = 0.1,
                                lwd.s = par("lwd"), lwd.mean = lwd.s,
                                main = "SI ratio",
                                xlab = NULL, ylab = NULL,
                                xlim = NULL, ylim = NULL,
                                ...) {
    data <- data_siratio(x, labels = labels)
    labels <- data$labels
    data_plot <- data$data_plot
    data_means <- data$data_means
    if (is.null(xlim))
        xlim <- c(0.55, length(labels) + 0.45)
    if (is.null(ylim))
        ylim <- range(x, na.rm = TRUE)
    if (!add){
        plot.new( )
        plot.window(
            xlim = xlim,
            ylim = ylim,
            xaxt = "n")
        axis(1, at = seq_along(labels), labels = labels)
        axis(2)
        if (box)
            box()
        title(main = main, xlab = xlab, ylab = ylab)
    }
    segments(x0 = data_means$x0, y0 = data_means$y0,
             x1 = data_means$x1, y1 = data_means$y1,
             col = col.mean, lwd = lwd.mean)
    for (i in labels) {
        sub <- data_plot$cycle == i
        lines(data_plot[sub, "x"], data_plot[sub, "s"], 
              lwd = lwd.s,
              col = col.s, ...
        )
        points(data_plot[sub, "x"], data_plot[sub, "si"], 
               pch = 1, cex = cex.i,
               col = col.i,
               ...
        )
    }
}
#' @rdname siratio
#' @export
ggsiratioplot <- function(x, labels = NULL,
                          col.s = "darkblue", col.i = "gray", col.mean = "red",
                          cex.i = 0.5,
                          lwd.s = 1, lwd.mean = lwd.s,
                          main = "SI ratio",
                          xlab = NULL, ylab = NULL,
                          ...) {
    UseMethod("ggsiratioplot", x)
}
#' @export
ggsiratioplot.SA <- function(x, ...){
    ggsiratioplot(siratio(x), ...)
}
#' @export
ggsiratioplot.jSA <- function(x, ...){
    ggsiratioplot(siratio(x), ...)
}
#' @export
ggsiratioplot.default <- function(x, labels = NULL,
                                  col.s = "darkblue", col.i = "gray", col.mean = "red",
                                  cex.i = 0.5,
                                  lwd.s = NULL, lwd.mean = lwd.s,
                                  main = "SI ratio",
                                  xlab = NULL, ylab = NULL,
                                  ...) {
    data <- data_siratio(x, labels = labels)
    labels <- data$labels
    data_plot <- data$data_plot
    data_means <- data$data_means
    ggplot2::ggplot(data = data_plot, ggplot2::aes(x = x, group = cycle)) +
        ggplot2::geom_segment(ggplot2::aes(x=x0, y = y0,
                                           xend = x1, yend = y1),
                              data=data_means, 
                              colour=col.mean,
                              lwd = lwd.mean) + 
        ggplot2::geom_line(ggplot2::aes(y=s), colour=col.s, lwd = lwd.s) + 
        ggplot2::geom_point(ggplot2::aes(y=si), colour=col.i, cex = cex.i) + 
        ggplot2::labs(title = main, 
                      x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(breaks = seq_along(labels), 
                                    labels = labels) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

data_siratio <- function(x, labels = NULL) {
    times <- time(x)
    if (is.null(labels)) {
        if (frequency(x)==12){
            labels <- month.abb
        } else if (frequency(x)==4){
            labels <- c("Q1", "Q2", "Q3", "Q4")
        } else if (frequency(x)==2) {
            labels <- c("H1","H2")
        } else {
            labels=c("")
        }
    }
    means <- tapply(x[,"s"], cycle(x), mean)
    data_means <- data.frame(x0 = seq_along(labels) - 0.45, y0 = means,
                             x1 = seq_along(labels) + 0.45, y1 = means,
                             cycle = factor(labels, levels = labels, ordered = TRUE)
    )
    scale <- 1/diff(range(times, na.rm = TRUE)) * 0.9
    data_plot <- data.frame(x = as.numeric((times - min(times)) * scale - 0.45 + cycle(x)),
                            s = as.numeric(x[, "s"]),
                            si = as.numeric(x[, "si"]),
                            cycle = factor(labels[cycle(x)], levels = labels, ordered = TRUE)
    )
    list(labels = labels, data_means = data_means,
         data_plot = data_plot)
}
utils::globalVariables(c("s", "si", "x0", "x1", "y0", "y1"))

