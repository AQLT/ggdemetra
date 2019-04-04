#'Seasonal adjustment time series
#'
#'@export
geom_text_arima <- function(mapping = NULL, data = NULL, stat = "arima",
                            position = "identity", ...,
                            method = c("x13", "tramoseats"), frequency = 12,
                            component = "sa",
                            spec = NULL,
                            x_arima = NULL, y_arima = NULL,
                            show.legend = NA, 
                            inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_arima
#' @name geom_text_arima
#' @export
geom_label_arima <- function(mapping = NULL, data = NULL, stat = "arima",
                             position = "identity", ...,
                             method = c("x13", "tramoseats"), frequency = 12,
                             component = "sa",
                             spec = NULL,
                             x_arima = NULL, y_arima = NULL,
                             show.legend = NA, 
                             inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLabel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_arima
#' @name geom_text_arima
#'@export
geom_text_repel_arima <- function(mapping = NULL, data = NULL, stat = "arima",
                                  position = "identity", ...,
                                  method = c("x13", "tramoseats"), frequency = 12,
                                  component = "sa",
                                  spec = NULL,
                                  x_arima = NULL, y_arima = NULL,
                                  show.legend = NA, 
                                  inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomTextRepel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_arima
#' @name geom_text_arima
#' @export
geom_label_repel_arima <- function(mapping = NULL, data = NULL, stat = "arima",
                                   position = "identity", ...,
                                   method = c("x13", "tramoseats"), frequency = 12,
                                   component = "sa",
                                   spec = NULL,
                                   x_arima = NULL, y_arima = NULL,
                                   show.legend = NA, 
                                   inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLabelRepel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_arima
#' @name geom_text_arima
#' @export
StatArima <- ggproto("StatArima", Stat, 
                     required_aes = c("x", "y"),
                     default_aes = aes(x = x, y = y, label = stat(arima_model)),
                     compute_group = function(data, scales,
                                              method = c("x13","tramoseats"), 
                                              frequency = 12,
                                              spec = NULL,
                                              x_arima = NULL, y_arima = NULL) {
                         method <- match.arg(method)
                         data_ts <- ts(data$y, start = data$x[1], frequency = frequency)
                         if (method == "x13") {
                             if (is.null(spec)) {
                                 sa <- RJDemetra::jx13(data_ts)
                             }else{
                                 sa <- RJDemetra::jx13(data_ts, spec = spec)
                             }
                         }else{
                             if (is.null(spec)) {
                                 sa <- RJDemetra::jtramoseats(data_ts)
                             }else{
                                 sa <- RJDemetra::jtramoseats(data_ts, spec = spec)
                             }
                         }
                         arima_model <- RJDemetra::get_indicators(sa,
                                                                  sprintf("preprocessing.arima.%s",
                                                                          c("p","d","q","bp","bd","bq")))
                         
                         arima_model <- paste0("ARIMA(",arima_model[[1]], ",",
                                               arima_model[[2]], ",",
                                               arima_model[[3]], ")(",
                                               arima_model[[4]], ",",
                                               arima_model[[5]], ",",
                                               arima_model[[6]], ")")
                         data <- data[1, ]
                         if (!is.null(x_arima))
                             data$x <- x_arima
                         if (!is.null(y_arima))
                             data$y <- y_arima
                         data$arima_model <- arima_model
                         data
                     }
)
#' @export
geom_label_arima <- function(mapping = NULL, data = NULL, geom = "line",
                             position = "identity", na.rm = FALSE, show.legend = NA, 
                             inherit.aes = TRUE,
                             method = c("x13","tramoseats"), frequency = 12,
                             spec = NULL, x_arima = NULL, y_arima = NULL,
                             ...) {
    ggplot2::layer(data = data, mapping = mapping, stat = StatArima, geom = GeomLabel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, x_arima = x_arima,
                                 y_arima = y_arima,
                                 ...))
}
#' @export
geom_text_arima <- function(mapping = NULL, data = NULL, geom = "line",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE,
                            method = c("x13","tramoseats"), frequency = 12,
                            spec = NULL, x_arima = NULL, y_arima = NULL,
                            ...) {
    ggplot2::layer(data = data, mapping = mapping, stat = StatArima, geom = GeomText, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, x_arima = x_arima,
                                 y_arima = y_arima,
                                 ...))
}


