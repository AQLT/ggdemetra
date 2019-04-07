#' @importFrom stats ts is.mts is.ts time
seasonal_adjustment <- function(data,
                                method = c("x13","tramoseats"),
                                spec = NULL,
                                frequency = NULL,
                                message = TRUE){
    method <- match.arg(method)
    data <- data[order(data$x), ]
    

    data_ts <- dataframe2ts(data = data, frequency = frequency, message = message)
    
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
    data$sa_model <- list(sa)
    list(data = data, sa = sa, dates = as.numeric(time(data_ts)), frequency = frequency)
}
dataframe2ts <- function(data, frequency = NULL, message = TRUE){
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
    
    ts(data$y, start = first_date, frequency = frequency)
}
ts2dataframe <- function(x){
    if (is.ts(x) & !is.mts(x)) {
        data.frame(x = as.numeric(time(x)),
                   y = as.numeric(x))
    }else{
        NULL
    }
   
}