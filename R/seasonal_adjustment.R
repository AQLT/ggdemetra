seasonal_adjustment <- function(data,
                                method = c("x13","tramoseats"),
                                spec = NULL,
                                frequency = NULL,
                                message = TRUE){
    method <- match.arg(method)
    data <- data[order(data$x), ]
    dates <- data$x
    
    if (class(dates) == "Date") {
        years <- as.numeric(format(dates, format = "%Y"))
        months <- as.numeric(format(dates, format = "%m"))
        if (is.null(frequency)) {
            frequency <- max(table(years))
            if (message)
                message(sprintf("Frenquency used: %i", frequency))
        }
        dates <- years + (months - 1) / frequency
        first_date <- dates[1]
    }else{
        # Numeric format
        if (is.null(frequency)) {
            years <- trunc(round(dates, 3))
            frequency <- max(table(years))
            if (message)
                message(sprintf("Frenquency used: %i", frequency))
        }
        first_date <- dates[1]
    }
    if (!frequency %in% c(2, 4, 6, 12))
        stop("Couldn't pick automatically the frequency: you must specify manually the argument 'frequency'")

    data_ts <- ts(data$y, start = first_date, frequency = frequency)
    
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
    
    list(data = data, sa = sa, dates = dates, frequency = frequency)
}