#' Convert 'ts' object to 'data.frame'
#' 
#' Function to a \code{ts} or \code{mts} object to a \code{data.frame} that can be directly used in the plot functions.
#' 
#' @param x a \code{ts} or \code{mts} object.
#' 
#' @return a \code{data.frame} object.
#' @examples 
#' # To get the ipi_c_eu_df object:
#' ts2df(ipi_c_eu)
#' @name ts2df
#' @rdname ts2df
#' @export
ts2df <- function(x){
    UseMethod("ts2df", x)
}
#' @export
ts2df.ts <- function(x){
    date <- as.numeric(time(x))
    name <- deparse(substitute(x))
    result <- data.frame(date = date,
               as.numeric(x))
    colnames(result) <- c("date", name)
    result
}
#' @export
ts2df.mts <- function(x){
    date <- as.numeric(time(x))
    data <- as.matrix(x)
    rownames(data) <- NULL
    data.frame(date = date,
               data)
}
