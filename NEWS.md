# ggdemetra 0.2.2

* `ts2df` function added to convert `ts` object to `data.frame`.

* data updated.

* `geom_outlier` bug correction (`first_date` and `last_date` not correctly working when `coefficients=TRUE`).

# ggdemetra 0.2.1

* `geom_arima` bug correction (a new model was computed).

# ggdemetra 0.2.0

* If no new data or seasonal adjustment specification is specified (method or specification), these parameters is inherited from the previous defined (the seasonal adjustment is then only done once).


