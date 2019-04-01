library(ggplot2)
StatSa <- ggproto("StatSa", Stat, 
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales,
                                           method = c("x13","tramoseats"), 
                                           frequency = 12,
                                           component = c("sa", "t"),
                                           spec = NULL) {
                      method <- match.arg(method)
                      component <- match.arg(component)
                      data_ts <- ts(data$y, start = data$x[1], frequency = frequency)
                      if (method == "x13") {
                          if (is.null(spec)) {
                              sa <- RJDemetra::x13(data_ts)
                          }else{
                              sa <- RJDemetra::x13(data_ts, spec = spec)
                          }
                      }else{
                          if (is.null(spec)) {
                              sa <- RJDemetra::tramoseats(data_ts)
                          }else{
                              sa <- RJDemetra::tramoseats(data_ts, spec = spec)
                          }
                      }
                      final_data <- as.data.frame.ts(sa$final$series[, c("sa", "t")])
                      data$y <- final_data[, component]
                      #data$sa <- final_data[, component]
                      cbind(data, final_data)
                  }
)

stat_sa <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA, 
                    inherit.aes = TRUE, method = c("x13","tramoseats"), frequency = 12,
                    component = c("sa", "t"),
                    spec = NULL,
                    ...) {
    ggplot2::layer(
        stat = StatSa, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(method = method, frequency = frequency, component = component,
                      spec = spec, ...)
    )
}
   
library(RJDemetra)
myseries <- ipi_c_eu[, "FR"]
myseries_data <- data.frame(x = as.numeric(time(ipi_c_eu)),
                            y = as.numeric(ipi_c_eu[, "FR"]))

myseries_data2 <- rbind(data.frame(x = as.numeric(time(ipi_c_eu)),
                                   y = as.numeric(ipi_c_eu[, "FR"]),
                                   serie = "FR", stringsAsFactors = FALSE),
                        data.frame(x = as.numeric(time(ipi_c_eu)),
                                   y = as.numeric(ipi_c_eu[, "IT"]),
                                   serie = "IT", stringsAsFactors = FALSE))

p <- ggplot(myseries_data, aes(x, y)) + 
    geom_line() + 
    stat_sa(colour = "red", component = "sa", spec = "RSA0") 
p

p <- ggplot(myseries_data2, aes(x, y, group = serie, color = serie)) + 
    geom_line() + 
    stat_sa(colour = "black",component = "sa", spec = "RSA0") 
p
