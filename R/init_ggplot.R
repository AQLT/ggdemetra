#' Initialise 'ggplot2' with 'SA' model
#' 
#' @param x A `"SA"` or `"jsA"` model created with 'RJDemetra'.
#' @param ... Other parameters passes to [ggplot2::ggplot()]
#' @examples
#' mod <- RJDemetra::x13(ipi_c_eu[, "FR"])
#' init_ggplot(mod) +
#'     geom_line(color =  "#F0B400") +
#'     geom_sa(component = "sa", color = "#155692")
#' @export
init_ggplot <- function(x, ...) {
    UseMethod("init_ggplot", x)
}
#' @export
init_ggplot.SA <- function(x, ...) {
    y <- raw(x)
    d_y <- ts2dataframe(y)
    if (inherits(x, "X13")) {
        spec = RJDemetra::x13_spec(x)
        method = "x13"
    } else {
        spec = RJDemetra::tramoseats_spec(x)
        method = "tramoseats"
    }
    seasonal_adjustment(data = d_y,
                        method = method,
                        spec = spec,
                        frequency = frequency(y),
                        message = FALSE,
                        new_data = TRUE)
    ggplot2::ggplot(
        data = d_y, 
        ggplot2::aes(x = x, y = y), 
        ...)
}
#' @export
init_ggplot.jSA <- function(x, ...) {
    init_ggplot(RJDemetra::jSA2R(x), ...)
}
