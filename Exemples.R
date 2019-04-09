devtools::build_vignettes()
library(pkgdown)
build_favicon(pkg = ".")
usethis::use_github_links(overwrite = TRUE)
template_reference()
template_articles()
pkgdown::template_navbar()
pkgdown::build_site()
pkgdown::build_home()

library(RJDemetra)
library(ggrepel)
library(ggplot2)
library(ggrepel)
file <- "docs/index.html"
index <- readLines(file)
i_to_change <- grep("Mes%20Documents", index)
index[i_to_change]
index <- gsub("../../Mes%20Documents/ggdemetra/man","reference",
              index,fixed = TRUE)
index[i_to_change]
writeLines(index, file)

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
p + geom_diagnostics(xmin = 2003, xmax =  2010,
                     ymin = 60, ymax = 70,
                     table_theme = ttheme_default(base_size = 10),
                     diagnostics =  c("diagnostics.td-res-all", "diagnostics.qs.on.i"))
diagnostics <- c(toto = "diagnostics.td-res-all", "diagnostics.qs.on.i",
                 "diagnostics.seas-res-periodogram","diagnostics.msr-global",
                 "decomposition.tlen", "decomposition.slen", "mode")
data <- ggplot_build(p)$data[[1]]
t <- data$sa_model

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