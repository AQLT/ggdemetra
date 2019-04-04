
StatDiagnostics <- ggproto("StatDiagnostics", Stat, 
                       required_aes = c("x", "y"),
                       default_aes = aes(x = x, y = y, label = stat(diagnostics)),
                       compute_group = function(data, scales,
                                                method = c("x13", "tramoseats"), 
                                                spec = NULL,
                                                frequency = NULL,
                                                message = TRUE,
                                                coefficients = FALSE,
                                                digits = 2,
                                                first_date = NULL,
                                                last_date = NULL){
                           result <- seasonal_adjustment(data = data,
                                                         method = method,
                                                         spec = spec,
                                                         frequency = frequency,
                                                         message = message)
                           data <- result[["data"]]
                           sa <- result[["sa"]]
                           frequency <- result[["frequency"]]
                           diagnostics <- c("diagnostics.td-res-all", "diagnostics.qs.on.i")
                           
                           diagnostics_table <- RJDemetra::get_indicators(sa, diagnostics)
                           diagnostics_table <- t(simplify2array(diagnostics_table))[,1, drop = FALSE]
                           diagnostics_table <- round(diagnostics_table, digits)
                           colnames(diagnostics_table) <- "P-value"
                           
                           id_date <- match(as.character(round(date, 3)),
                                            as.character(round(result[["dates"]], 3)))
                           data_final <- data.frame(x = data$x[id_date],
                                                    y = data$y[id_date],
                                                    outlier =  label_outlier,
                                                    stringsAsFactors = FALSE
                           )
                           data_final
                       }
)

annotation_custom
#' Seasonal adjustment time series
#'
#' @export
geom_text_outlier <- function(mapping = NULL, data = NULL, stat = "outlier",
                              position = "identity", ...,
                              method = c("x13", "tramoseats"), 
                              spec = NULL,
                              frequency = NULL,
                              message = TRUE,
                              coefficients = FALSE,
                              digits = 1,
                              show.legend = NA, 
                              inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_outlier
#' @name geom_text_outlier
#' @export
geom_label_outlier <- function(mapping = NULL, data = NULL, stat = "outlier",
                               position = "identity", ...,
                               method = c("x13", "tramoseats"), 
                               spec = NULL,
                               frequency = NULL,
                               message = TRUE,
                               coefficients = FALSE,
                               digits = 1,
                               show.legend = NA, 
                               inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLabel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_outlier
#' @name geom_text_outlier
#' @export
geom_text_repel_outlier <- function(mapping = NULL, data = NULL, stat = "outlier",
                                    position = "identity", ...,
                                    method = c("x13", "tramoseats"), 
                                    spec = NULL,
                                    frequency = NULL,
                                    message = TRUE,
                                    coefficients = FALSE,
                                    digits = 1,
                                    show.legend = NA, 
                                    inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomTextRepel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_outlier
#' @name geom_text_outlier
#' @export
geom_label_repel_outlier <- function(mapping = NULL, data = NULL, stat = "outlier",
                                     position = "identity", ...,
                                     method = c("x13", "tramoseats"), 
                                     spec = NULL,
                                     frequency = NULL,
                                     message = TRUE,
                                     coefficients = FALSE,
                                     digits = 1,
                                     show.legend = NA, 
                                     inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLabelRepel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 coefficients = coefficients, digits = digits,
                                 ...))
}