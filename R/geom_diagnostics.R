#' Seasonal adjustment time series
#'
#' @inheritParams geom_sa
#' @param diagnostics vector of character containing the name of the diagnostics to plot.
#'    See [user_defined_variables()][RJDemetra::user_defined_variables] for the available
#'    parameters.
#' @param digits integer indicating the number of decimal places to be used for numeric diagnostics. By default `digits = 2`.
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster.
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster.
#' @param table_theme list of theme parameters for the table of diagnostics.
#' 
#' @importFrom gridExtra tableGrob ttheme_default
#' @export
geom_diagnostics <- function(mapping = NULL, data = NULL,
                             position = "identity", ...,
                             method = c("x13", "tramoseats"), 
                             spec = NULL,
                             frequency = NULL,
                             message = TRUE,
                             diagnostics = NULL,
                             digits = 2,
                             xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
                             table_theme = ttheme_default(),
                             inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = StatDiagnostics,
                   geom = GeomDiagnostics, 
                   position = position, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 digits = digits, diagnostics = diagnostics,
                                 xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                 table_theme = table_theme,
                                 ...))
}
GeomDiagnostics <- ggproto("GeomDiagnostics", Geom,
                         extra_params = "",
                         handle_na = function(data, params) {
                             data
                         },
                         draw_panel = function(data, panel_params, coord,
                                               xmin = -Inf, xmax = Inf,
                                               ymin = -Inf, ymax = Inf,
                                               table_theme = ttheme_default()) {
                             if (is.null(data))
                                 NULL
                             if (!inherits(coord, "CoordCartesian")) {
                                 stop("geom_diagnostics only works with Cartesian coordinates",
                                      call. = FALSE)
                             }
                             corners <- data.frame(x = c(xmin, xmax),
                                                   y = c(ymin, ymax))
                             datatemp <- coord$transform(corners, panel_params)
                             x_rng <- range(datatemp$x, na.rm = TRUE)
                             y_rng <- range(datatemp$y, na.rm = TRUE)
                             vp <- grid::viewport(x = mean(x_rng), y = mean(y_rng),
                                            width = diff(x_rng), height = diff(y_rng),
                                            just = c("center","center"))
                             
                             ## computation data
                             
                             grob <- gridExtra::tableGrob(data[, c("Diagnostic", "Value")],
                                                          theme = table_theme,
                                                          rows = NULL)
                             ##
                             grid::editGrob(grob, vp = vp)
                         },
                         default_aes = aes_(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
)

StatDiagnostics <- ggproto("StatDiagnostics", Stat, 
                       required_aes = c("x", "y"),
                       compute_group = function(data, scales,
                                                method = c("x13", "tramoseats"), 
                                                spec = NULL,
                                                frequency = NULL,
                                                message = TRUE,
                                                diagnostics = NULL,
                                                digits = 2,
                                                first_date = NULL,
                                                last_date = NULL){
                           if (is.null(diagnostics))
                               return(NULL)
                           result <- seasonal_adjustment(data = data,
                                                         method = method,
                                                         spec = spec,
                                                         frequency = frequency,
                                                         message = message)
                           data <- result[["data"]]
                           sa <- result[["sa"]]
                           frequency <- result[["frequency"]]

                           diag_table <- RJDemetra::get_indicators(sa, diagnostics)
                           diag_table <- lapply(diag_table, function(x){
                               if (is.null(x) || is.ts(x))
                                   return(NULL)
                               if (length(x) > 1) {
                                   x <- x[2]
                               }
                               if (is.numeric(x))
                                   x <- round(x, digits)
                               x
                           })
                           diag_table <- do.call(c, diag_table)
                           if (is.null(diag_table))
                               NULL
                           diag_names <- diagnostics[diagnostics %in% names(diag_table)]
                           if (!is.null(diag_names)) {
                               names_supplied <- names(diag_names) != ""
                               diag_names[names_supplied] <- names(diag_names)[names_supplied]
                           }
                           diag_table <- data.frame(Diagnostic =
                                                               names(diag_table),
                                                           Value = diag_table)
                           diag_table
                       }
)
