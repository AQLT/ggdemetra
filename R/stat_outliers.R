#'Seasonal adjustment time series
#'
#'@export
geom_text_outliers <- function(mapping = NULL, data = NULL, stat = "outliers",
                               position = "identity", ...,
                               method = c("x13", "tramoseats"), frequency = 12,
                               component = "sa",
                               spec = NULL,
                               coefficients = FALSE,
                               digits = 1,
                               show.legend = NA, 
                               inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_outliers
#' @name geom_text_outliers
#' @export
geom_label_outliers <- function(mapping = NULL, data = NULL, stat = "outliers",
                                position = "identity", ...,
                                method = c("x13", "tramoseats"), frequency = 12,
                                component = "sa",
                                spec = NULL,
                                coefficients = FALSE,
                                digits = 1,
                                show.legend = NA, 
                                inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLabel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_outliers
#' @name geom_text_outliers
#'@export
geom_text_repel_outliers <- function(mapping = NULL, data = NULL, stat = "outliers",
                                     position = "identity", ...,
                                     method = c("x13", "tramoseats"), frequency = 12,
                                     component = "sa",
                                     spec = NULL,
                                     coefficients = FALSE,
                                     digits = 1,
                                     show.legend = NA, 
                                     inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomTextRepel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_outliers
#' @name geom_text_outliers
#' @export
geom_label_repel_outliers <- function(mapping = NULL, data = NULL, stat = "outliers",
                                      position = "identity", ...,
                                      method = c("x13", "tramoseats"), frequency = 12,
                                      component = "sa",
                                      spec = NULL,
                                      coefficients = FALSE,
                                      digits = 1,
                                      show.legend = NA, 
                                      inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLabelRepel, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, frequency = frequency,
                                 spec = spec, coefficients = coefficients, digits = digits,
                                 ...))
}
#' @rdname geom_text_outliers
#' @name geom_text_outliers
#' @export
stat_outliers <- function(mapping = NULL, data = NULL, geom = "line",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, method = c("x13","tramoseats"), frequency = 12,
                          spec = NULL,
                          coefficients = FALSE,
                          digits = 1,
                          ...) {
    
    ggplot2::layer(
        stat = StatOutlier, data = data, mapping = mapping, geom = GeomLabelRepel, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(method = method, frequency = frequency,
                      spec = spec, coefficients = coefficients, digits = digits,
                      ...)
    )
}

StatOutlier <- ggproto("StatOutlier", Stat, 
                       required_aes = c("x", "y"),
                       default_aes = aes(x = x, y = y, label = stat(outliers)),
                       compute_group = function(data, scales,
                                                method = c("x13","tramoseats"), 
                                                frequency = 12,
                                                spec = NULL,
                                                coefficients = FALSE,
                                                digits = 1) {
                           method <- match.arg(method)
                           data_ts <- ts(data$y, start = data$x[1], frequency = frequency)
                           data <- data
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
                           
                           reg_names <- RJDemetra::get_indicators(sa, "preprocessing.model.description")[[1]]
                           liste_outliers <- grep("(^LS )| (^AO )| (^TC )| (^SO )",
                                                  reg_names)
                           if(length(liste_outliers) == 0)
                               return(NULL)
                           liste_outliers_name <- reg_names[liste_outliers]
                           date <- gsub("(^.* )|(\\()|(\\))", "", liste_outliers_name)
                           date <- sapply(strsplit(date, "-"),function(x){
                               x <- as.numeric(x)
                               x[2] + (x[1] - 1)/frequency
                           })
                           label_outliers <- liste_outliers_name
                           if(coefficients){
                               reg_coef <- RJDemetra::get_indicators(sa, "preprocessing.model.coefficients")[[1]][liste_outliers,1]
                               label_outliers <- sprintf(paste0("%s: %.",digits,"f"),
                                                         liste_outliers_name,
                                                         reg_coef)
                           }
                           
                           id_date <- match(as.character(round(date,3)), as.character(round(data$x,3)))
                           data_final <- data.frame(x = data$x[id_date],
                                                    y = data$y[id_date],
                                                    outliers =  label_outliers,
                                                    stringsAsFactors = FALSE
                           )
                           data_final
                       }
)
