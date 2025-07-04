StatSa <- ggproto("StatSa", Stat, 
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales,
                                           method = c("x13", "tramoseats"), 
                                           spec = NULL,
                                           frequency = NULL,
                                           message = TRUE,
                                           component = "sa",
                                           new_data = TRUE) {
                      result <- seasonal_adjustment(data = data,
                                                    method = method,
                                                    spec = spec,
                                                    frequency = frequency,
                                                    message = message,
                                                    new_data = new_data)
                      data <- result[["data"]]
                      sa <- result[["sa"]]
                      component <- component[1]
                      component_ts <- RJDemetra::get_indicators(sa, component)[[1]]
                      if (!is.ts(component_ts)) {
                          warning(sprintf("The component %s isn't a time series!", component))
                          return(NULL)
                      }
                      component_df <- ts2dataframe(component_ts)
                      
                      # if the ts is a forecast we add the last observed value:
                      if (length(grep("^.*_f$", component)) > 0) {
                          component_df <- rbind(tail(data[,c("x", "y")],1), component_df)
                      }
                      
                      data$x <- data$y <- NULL
                      if (nrow(component_df) <= nrow(data)) {
                          component_df <- cbind(component_df,
                                                data[seq_len(nrow(component_df)), ])
                      }
                      
                      component_df
                  }
)
#' Seasonal adjustment time series
#' 
#' Performs a seasonal adjustment and plots a time series. 
#' `geom_sa()` and `stat_sa()` are  aliases: they both use the same arguments. 
#' Use `stat_sa()` if you want to display the results with a non-standard geom.
#' 
#' @param mapping Set of aesthetic mappings created by [aes()][ggplot2::aes]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data A \code{data.frame} that contains the data used for the seasonal adjustment.
#' @param geom The geometric object to use to display the data
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param ... Other arguments passed on to [layer()][ggplot2::layer]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`.
#' @param method the method used for the seasonal adjustment. `"x13"` (by default) for the X-13ARIMA method and `"tramoseats"` for TRAMO-SEATS.
#' @param spec the specification used for the seasonal adjustment. 
#'    See [x13()][RJDemetra::x13] or [tramoseats()][RJDemetra::tramoseats].
#' @param frequency the frequency of the time series. By default (`frequency = NULL`),
#'    the frequency is computed automatically.
#' @param message a `boolean` indicating if a message is printed with the frequency used.
#' @param component a `character` equals to the component to plot. The result must be a time series. 
#'    See [user_defined_variables()][RJDemetra::user_defined_variables] for the available
#'    parameters. By default (`component = 'sa'`) the seasonal adjusted component is plotted.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them.
#'   
#'   
#' @examples 
#' p_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
#'     geom_line(color =  "#F0B400") +
#'     labs(title = "Seasonal adjustment of the French industrial production index",
#'          x = "time", y = NULL)
#' 
#' # To add the seasonal adjusted series:
#' p_ipi_fr +
#'     geom_sa(color = "#155692")
#' 
#' # To add the forecasts of the input data and the seasonal adjusted series:
#' p_sa <- p_ipi_fr +
#'     geom_sa(component = "y_f", linetype = 2, message = FALSE, color =  "#F0B400") + 
#'     geom_sa(component = "sa", color = "#155692", message = FALSE) +
#'     geom_sa(component = "sa_f", color = "#155692", linetype = 2, message = FALSE)
#' p_sa 
#' @importFrom ggplot2 GeomLine
#' @export
geom_sa <- function(mapping = NULL, data = NULL, stat = "sa",
                    position = "identity", ...,
                    method = c("x13", "tramoseats"), 
                    spec = NULL,
                    frequency = NULL,
                    message = TRUE,
                    component = "sa",
                    show.legend = NA, 
                    inherit.aes = TRUE
) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLine, 
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                   params = list(method = method, spec = spec, 
                                 frequency = frequency, message = message,
                                 component = component,
                                 new_data = !missing(data) || !is.null(data),
                                 ...))
}
#' @rdname geom_sa
#' @name geom_sa
#' @export
stat_sa <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", ...,
                    method = c("x13", "tramoseats"), 
                    spec = NULL,
                    frequency = NULL,
                    message = TRUE,
                    component = "sa",
                    show.legend = NA, 
                    inherit.aes = TRUE) {
    ggplot2::layer(
        stat = StatSa, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(method = method, spec = spec, 
                      frequency = frequency, message = message,
                      component = component,
                      new_data = !missing(data) || !is.null(data),
                      ...)
    )
}
