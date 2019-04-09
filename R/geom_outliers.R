
StatOutlier <- ggproto("StatOutlier", Stat, 
                       required_aes = c("x", "y"),
                       default_aes = aes(x = x, y = y, label = stat(outlier)),
                       compute_group = function(data, scales,
                                                method = c("x13", "tramoseats"), 
                                                spec = NULL,
                                                frequency = NULL,
                                                message = TRUE,
                                                first_date = NULL,
                                                last_date = NULL,
                                                coefficients = FALSE,
                                                digits = 1){
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


#' Outliers texts
#'
#' Function to add directly to the plot the outliers used in the pre-adjustment process of the seasonal adjustment.
#'
#' @inheritParams geom_sa
#' @param geom character. The geometric to use to display the data: 
#'    `GeomText` (`geom = "text"`, the default, see [geom_text()][ggplot2::geom_text]); 
#'    `GeomLabel` (`geom = "label"`, see [geom_label()][ggplot2::geom_label]); 
#'    `GeomTextRepel` (`geom = "text_repel"`, the default, see [geom_text_repel()][ggrepel::geom_text_repel]); 
#'    `GeomLabelRepel` (`geom = "label_repel"`, the default, see [geom_label_repel()][ggrepel::geom_label_repel]).
#'   
#' @param ... Other arguments passed on to [layer()][ggplot2::layer]. They may be parameters of 
#'    [geom_text()][ggplot2::geom_text] (if `geom = "text"`), 
#'    [geom_label()][ggplot2::geom_label] (if `geom = "label"`), 
#'    [geom_text_repel()][ggrepel::geom_text_repel] (if `geom = "text_repel"`) 
#'    or [geom_label_repel()][ggrepel::geom_label_repel] (if `geom = "label_repel"`).
#' @param first_date A numeric specifying the first date from which the outliers are plotted.
#'    By default (`first_date = NULL`) the outliers are plotted from the 
#'    beginning of the time series.
#' @param last_date A numeric specifying the first date from which the outliers are plotted.
#'    By default (`first_date = NULL`) the outliers are plotted until the 
#'    end of the time series.
#' @param coefficients boolean indicating if the estimates coefficients are printed. 
#'    By default `coefficients = FALSE`.
#' @param digits integer indicating the number of decimal places to be used for numeric diagnostics. By default `digits = 1`. 
#' @details 
#' With the parameter `geom = "text"`, the outliers used in the pre-adjustment process of the seasonal adjustment are directly added to the plot. With `geom = "label"` a rectangle is drawn behind the names of the outliers, making them easier to read. The same with `geom = "text_repel"` or `geom = "label_repel"` but text labels are also repeled away from each other and away from the data points (see [geom_label_repel()][ggrepel::geom_label_repel]).
#'
#' @examples 
#' data <- data.frame(x = as.numeric(time(ipi_c_eu)),
#'                    y = as.numeric(ipi_c_eu[, "FR"]))
#' p_ipi_fr <- ggplot(data = data, mapping = aes(x = x, y = y)) +
#'     geom_line() +
#'     labs(title = "Seasonal adjustment of the French industrial production index",
#'          x = "time", y = NULL)
#'          
#' # To add the outliers:
#' p_ipi_fr + geom_outlier(geom = "label",
#'                     message = FALSE)
#' 
#' 
#' # To have a more readable plot with outliers names that repeled away from each other 
#' # and from the data points:
#' p_ipi_fr + 
#'     geom_outlier(geom = "label_repel",
#'                  message = FALSE,
#'                  vjust = 4,
#'                  ylim = c(NA, 65), force = 10,
#'                  arrow = arrow(length = unit(0.03, "npc"),
#'                                type = "closed", ends = "last"))
#' 
#' # To only plot the outliers from a specific date (2009):
#' p_ipi_fr + 
#'     geom_outlier(geom = "label_repel",
#'                  message = FALSE,
#'                  first_date = 2009,
#'                  vjust = 4,
#'                  ylim = c(NA, 65), force = 10,
#'                  arrow = arrow(length = unit(0.03, "npc"),
#'                                type = "closed", ends = "last"))
#' @export
geom_outlier <- function(mapping = NULL, data = NULL,
                              stat = "outlier",
                              geom = c("text", "label",
                                       "text_repel", "label_repel"),
                              position = "identity", ...,
                              method = c("x13", "tramoseats"), 
                              spec = NULL,
                              frequency = NULL,
                              message = TRUE,
                              first_date = NULL,
                              last_date = NULL,
                              coefficients = FALSE,
                              digits = 1,
                              show.legend = NA, 
                              inherit.aes = TRUE
) {
    geom <- match.arg(geom)
    if (geom == "label_repel") {
        geom <- GeomLabelRepel
    } else if (geom == "text_repel") {
        geom <- GeomTextRepel
    } else if (geom == "label") {
        geom <- GeomLabel
    } else {
        geom <- GeomText
    }

    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = geom, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 first_date = first_date, last_date = last_date,
                                 coefficients = coefficients, digits = digits,
                                 ...))
}

