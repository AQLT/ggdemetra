#' Extract Component from 'RJDemetra' model
#' 
#' @param x a \code{"SA"} or \code{"jSA"} model.
#' @param forecast boolean indicating if the forecast series should be returned.
#' @name components
#' @rdname components
#' @export
seasonal <- function(x, forecast = FALSE) {
    UseMethod("seasonal", x)
}
#' @export
seasonal.SA <- function(x, forecast = FALSE){
    if (forecast) {
        x$final$forecasts[,"s_f"]
    } else {
        x$final$series[,"s"]
    }
}
#' @export
seasonal.jSA <- function(x, forecast = FALSE){
    if (forecast) {
        get_indicators(x, "s_f")[[1]]
    } else {
        get_indicators(x, "s")[[1]]
    }
}
#' @rdname components
#' @export
trendcycle <- function(x, forecast = FALSE) {
    UseMethod("trendcycle", x)
}
#' @export
trendcycle.SA <- function(x, forecast = FALSE){
    if (forecast) {
        x$final$forecasts[,"t_f"]
    } else {
        x$final$series[,"t"]
    }
}
#' @export
trendcycle.jSA <- function(x, forecast = FALSE){
    if (forecast) {
        get_indicators(x, "t_f")[[1]]
    } else {
        get_indicators(x, "t")[[1]]
    }
}
#' @rdname components
#' @export
irregular <- function(x, forecast = FALSE) {
    UseMethod("irregular", x)
}
#' @export
irregular.SA <- function(x, forecast = FALSE){
    if (forecast) {
        x$final$forecasts[,"i_f"]
    } else {
        x$final$series[,"i"]
    }
}
#' @export
irregular.jSA <- function(x, forecast = FALSE){
    if (forecast) {
        get_indicators(x, "i_f")[[1]]
    } else {
        get_indicators(x, "i")[[1]]
    }
}
#' @rdname components
#' @export
seasonaladj <- function(x, forecast = FALSE) {
    UseMethod("seasonaladj", x)
}
#' @export
seasonaladj.SA <- function(x, forecast = FALSE){
    if (forecast) {
        x$final$forecasts[,"sa_f"]
    } else {
        x$final$series[,"sa"]
    }
}
#' @export
seasonaladj.jSA <- function(x, forecast = FALSE){
    if (forecast) {
        get_indicators(x, "sa_f")[[1]]
    } else {
        get_indicators(x, "sa")[[1]]
    }
}
#' @rdname components
#' @export
calendaradj <- function(x, forecast = FALSE) {
    UseMethod("calendaradj", x)
}
#' @export
calendaradj.SA <- function(x, forecast = FALSE){
    y <- get_ts(x)
    if (inherits(x, "X13")) {
        jmod <- jx13(y, x13_spec(x))
    } else {
        jmod <- jx13(y, tramoseats_spec(x))
    }
    calendaradj(jmod, forecast = forecast)
}
#' @export
calendaradj.jSA <- function(x, forecast = FALSE){
    if (forecast) {
        get_indicators(x, "preprocessing.model.ycal_f")[[1]]
    } else {
        get_indicators(x, "preprocessing.model.ycal")[[1]]
    }
}

#' @rdname components
#' @export
calendar <- function(x, forecast = FALSE) {
    UseMethod("calendar", x)
}
#' @export
calendar.SA <- function(x, forecast = FALSE){
    y <- get_ts(x)
    if (inherits(x, "X13")) {
        jmod <- jx13(y, x13_spec(x))
    } else {
        jmod <- jx13(y, tramoseats_spec(x))
    }
    calendar(jmod, forecast = forecast)
}
#' @export
calendar.jSA <- function(x, forecast = FALSE){
    if (forecast) {
        get_indicators(x, "preprocessing.model.cal_f")[[1]]
    } else {
        get_indicators(x, "preprocessing.model.cal")[[1]]
    }
}
#' @rdname components
#' @export
y_forecast <- function(x) {
    UseMethod("y_forecast", x)
}
#' @export
y_forecast.SA <- function(x) {
    y <- get_ts(x)
    if (inherits(x, "X13")) {
        jmod <- jx13(y, x13_spec(x))
    } else {
        jmod <- jx13(y, tramoseats_spec(x))
    }
    y_forecast(jmod)
}
#' @export
y_forecast.jSA <- function(x) {
    get_indicators(x, "y_f")[[1]]
}
