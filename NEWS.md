# ggdemetra 0.2.6

* `siratio()` correction with TRAMO-SEATS `"jSA"` models.

* `y_forecast()` replaces by `raw()`

# ggdemetra 0.2.5

* new functions: `siratio()`, `siratioplot()` and `ggsiratioplot()` to plot SI ratios.

* SystemRequirements update for CRAN policies.

# ggdemetra 0.2.3

* new functions: `autoplot.SA()` and `autoplot.jSA()` to plot 'RJDemetra' models and `y_forecast()`, ` trendcycle()`, ` seasonaladj()`, ` calendaradj()`, ` seasonal()`, ` irregular()`, ` calendar()` to extract different components of the models.

* `ggdemetra` now depends on `RJDemetra`.

# ggdemetra 0.2.2

* `ts2df` function added to convert `ts` object to `data.frame`.

* data updated.

* `geom_outlier` bug correction (`first_date` and `last_date` not correctly working when `coefficients=TRUE`).

# ggdemetra 0.2.1

* `geom_arima` bug correction (a new model was computed).

# ggdemetra 0.2.0

* If no new data or seasonal adjustment specification is specified (method or specification), these parameters is inherited from the previous defined (the seasonal adjustment is then only done once).


