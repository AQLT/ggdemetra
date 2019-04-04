StatSa <- ggproto("StatSa", Stat, 
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales,
                                           method = c("x13", "tramoseats"), 
                                           spec = NULL,
                                           frequency = NULL,
                                           message = TRUE,
                                           component = "sa") {
                      result <- seasonal_adjustment(data = data,
                                                    method = method,
                                                    spec = spec,
                                                    frequency = frequency,
                                                    message = message)
                      data <- result[["data"]]
                      sa <- result[["sa"]]
                      component_ts <- RJDemetra::get_indicators(sa, component)[[1]]
                      if (!is.ts(component_ts)) {
                          warning(sprintf("The component %s isn't a time series!", component))
                          return(NULL)
                      }
                      data$y <- as.numeric(component_ts)
                      data
                  }
)
#'Seasonal adjustment time series
#'
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
                                 component = component, ...))
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
                      component = component, ...)
    )
}
