
StatOutlier <- ggproto("StatOutlier", Stat, 
                       required_aes = c("x", "y"),
                       default_aes = aes(x = x, y = y, label = stat(outlier)),
                       compute_group = function(data, scales,
                                                method = c("x13", "tramoseats"), 
                                                spec = NULL,
                                                frequency = NULL,
                                                message = TRUE,
                                                coefficients = FALSE,
                                                digits = 1,
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
                           
                           reg_names <- RJDemetra::get_indicators(sa, "preprocessing.model.description")[[1]]
                           liste_outlier <- grep("(^LS )| (^AO )| (^TC )| (^SO )",
                                                  reg_names)
                           
                           if (length(liste_outlier) == 0)
                               return(NULL)
                           liste_outlier_name <- reg_names[liste_outlier]
                           
                           # Extraction of the date
                           date <- gsub("(^.* )|(\\()|(\\))", "", liste_outlier_name)
                           date <- sapply(strsplit(date, "-"),function(x){
                               x <- as.numeric(x)
                               x[2] + (x[1] - 1)/frequency
                           })
                           
                           out_to_keep <- 1:length(date)
                           if (!is.null(first_date))
                               out_to_keep <- intersect(out_to_keep, which(date >= first_date))
                           if (!is.null(last_date))
                               out_to_keep <- intersect(out_to_keep, which(date <= last_date))
                           
                           date <- date[out_to_keep]
                           liste_outlier_name <- liste_outlier_name[out_to_keep]
                           
                           if (length(liste_outlier) == 0)
                               return(NULL)
                           
                           label_outlier <- liste_outlier_name
                           if (coefficients) {
                               reg_coef <- RJDemetra::get_indicators(sa, "preprocessing.model.coefficients")[[1]][liste_outlier,1]
                               label_outlier <- sprintf(paste0("%s: %.",digits,"f"),
                                                         liste_outlier_name,
                                                         reg_coef)
                           }
                           
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

# stat_outlier <- function(mapping = NULL, data = NULL, geom = "line",
#                           position = "identity", na.rm = FALSE, show.legend = NA, 
#                           inherit.aes = TRUE, method = c("x13","tramoseats"), frequency = 12,
#                           spec = NULL,
#                           coefficients = FALSE,
#                           digits = 1,
#                           ...) {
#     
#     ggplot2::layer(
#         stat = StatOutlier, data = data, mapping = mapping, geom = GeomLabelRepel, 
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(method = method, frequency = frequency,
#                       spec = spec, coefficients = coefficients, digits = digits,
#                       ...)
#     )
# }
