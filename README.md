
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggdemetra

[![Build
Status](https://api.travis-ci.com/AQLT/ggdemetra.svg?branch=master)](https://travis-ci.com/AQLT/ggdemetra)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ggdemetra)](https://cran.r-project.org/package=ggdemetra)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg?logo=github)](https://www.tidyverse.org/lifecycle/#experimental)

## Overview

ggdemetra is an extension of
[ggplot2](https://github.com/tidyverse/ggplot2) to add seasonal
adjustment statistics to your plots. The seasonal adjustment process is
done with (RJDemetra)\[<https://github.com/jdemetra/rjdemetra>\] that is
an R interface to [JDemetra+](https://github.com/jdemetra/jdemetra-app),
the seasonal adjustment software [officially
recommended](https://ec.europa.eu/eurostat/cros/system/files/Jdemetra_%20release.pdf)
to the members of the European Statistical System (ESS) and the European
System of Central Banks. RJDemetra implements the two leading seasonal
adjustment methods
[TRAMO/SEATS+](http://www.bde.es/bde/en/secciones/servicios/Profesionales/Programas_estadi/Programas_estad_d9fa7f3710fd821.html)
and [X-12ARIMA/X-13ARIMA-SEATS](https://www.census.gov/srd/www/x13as/).

## Installation

Since RJDemetra relies on the
[rJava](https://CRAN.R-project.org/package=rJava) package and Java SE 8
or later version is required, the same requirements are also needed for
ggdemetra.

ggdemetra is not on CRAN for the moment and uses the development version
of RJDemetra. To use it you need to install both GitHub version:

``` r
# install.packages("devtools")
devtools::install_github("jdemetra/rjdemetra")
devtools::install_github("AQLT/ggdemetra")
```
