StatArima <- ggproto("StatArima", Stat, 
                     required_aes = c("x", "y"),
                     default_aes = aes(x = x, y = y, label = stat(arima_model)),
                     compute_group = function(data, scales,
                                              method = c("x13", "tramoseats"), 
                                              spec = NULL,
                                              frequency = NULL,
                                              message = TRUE,
                                              x_arima = NULL, y_arima = NULL) {
                         result <- seasonal_adjustment(data = data,
                                                       method = method,
                                                       spec = spec,
                                                       frequency = frequency,
                                                       message = message)
                         data <- result[["data"]]
                         sa <- result[["sa"]]
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
#' ARIMA model
#' 
#' \code{geom_text_arima()} adds the ARIMA models directly to the plot. \code{geom_label_arima()} draws a rectangle behind the ARIMA model, making it easier to read.
#' @export
geom_text_arima <- function(mapping = NULL, data = NULL, stat = "arima",
                            position = "identity", ...,
                            method = c("x13", "tramoseats"), 
                            spec = NULL,
                            frequency = NULL,
                            x_arima = NULL, y_arima = NULL,
                            show.legend = NA, 
                            inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 x_arima = x_arima, y_arima = y_arima,
                                 ...))
}
#' @rdname geom_text_arima
#' @name geom_text_arima
#' @export
geom_label_arima <- function(mapping = NULL, data = NULL, stat = "arima",
                             position = "identity", ...,
                             method = c("x13", "tramoseats"), 
                             spec = NULL,
                             frequency = NULL,
                             message = TRUE,
                             x_arima = NULL, y_arima = NULL,
                             show.legend = NA, 
                             inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 x_arima = x_arima, y_arima = y_arima,
                                 ...))
}
