StatSa <- ggproto("StatSa", Stat, 
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales,
                                           method = c("x13","tramoseats"), 
                                           frequency = 12,
                                           component = "sa",
                                           spec = NULL) {
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
                      component_ts <- RJDemetra::get_indicators(sa, component)[[1]]
                      if (!is.ts(component_ts)) {
                          warning(sprintf("The component %s isn't a time series!", component))
                          return(NULL)
                      }
                      data$y <- as.numeric(component_ts)
                      data
                  }
)

#'@export
stat_sa <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA, 
                    inherit.aes = TRUE, method = c("x13","tramoseats"), frequency = 12,
                    component = "sa",
                    spec = NULL,
                    ...) {
    ggplot2::layer(
        stat = StatSa, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(method = method, frequency = frequency, component = component,
                      spec = spec, ...)
    )
}

