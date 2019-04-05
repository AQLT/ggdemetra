library(RJDemetra)
library(ggrepel)
library(ggplot2)
library(ggrepel)
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
    geom_point() + 
    geom_sa(colour = "red", component = "sa", spec = "RSA0") 
p
data <- ggplot_build(p)$data[[1]]

p <- ggplot(myseries_data2, aes(x, y, group = serie, color = serie)) + 
    geom_line() + 
    stat_sa(colour = "black",component = "sa", spec = "RSA0") 
p


p <- ggplot(myseries_data, aes(x, y)) + 
    geom_line() + 
    stat_outliers(colour = "red", spec = "RSA3",
                  direction = "y",force = 20,
                  label.size = 0.15,
                  arrow = arrow(length = unit(0.01, 'npc')),
                  coefficient = TRUE) 

p
data <- ggplot_build(p)$data[[1]]
id_date <- match(as.character(round(date,3)), as.character(round(data[[1]]$x,3)))
data[[1]][id_date,]
data[[2]]