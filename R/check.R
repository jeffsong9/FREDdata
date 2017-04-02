FREDCPI <- read.csv("Data/FREDCPI.csv", stringsAsFactors = F)

#' Obtain the closest date entry in the FRED data set
#'
#' @param x Initial date that one wishes to match with the closest date on FRED data set. \code{x} must be in ymd format, e.g., "1988-09-23".
#' @export
#' @return Return the FRED data with the date that is closest to the initial date \code{x}.
#' @examples
#' convert_values("2017-03-30", 100, "1988-09-23")

closest_date=function(x){
  y=ymd(FREDCPI$DATE)
  FREDCPI[which(abs(y-ymd(x))==min(abs(y-ymd(x)))),] %>%
    return()
}

#' Convert values
#'
#' @param x Date for the initial entry. \code{x} must be in ymd format, e.g., "2017-03-30".
#' @param y Value for the initial entry.
#' @param z Date for the target entry. \code{z} must be in ymd format, e.g., "1988-09-23".
#' @export
#' @return Return the inflation incorporated price of the initial price \code{y}, given the initial date \code{x} and target date \code{z}.
#' @examples
#' convert_values("2017-03-30", 100, "1988-09-23")



# convert_values=function(initial_day, initial_value, target_day){
#   adj_initial=closest_date(initial_day)
#   adj_target=closest_date(target_day)
#   target_value=as.numeric(initial_value)*adj_target$CPIAUCSL/adj_initial$CPIAUCSL
#   return(target_value)
# }


convert_values=function(x, y, z){
  adj_initial=closest_date(x)
  adj_target=closest_date(z)
  target_value=as.numeric(y)*adj_target$CPIAUCSL/adj_initial$CPIAUCSL
  return(target_value)
}

#'Plot Consumer Price Index Through Time
#'
#'@param x Date for the initial entry
#'@param y Date for the final entry
#'@export
#' @return Plot the calculated CPI for All Urban Consumers \code{x} until \code{y}.
#' @examples
#' inflation.through.time("1975-01-01","1985-01-01")

inflation.through.time <- function(x,y){
  FREDCPI$DATE <- ymd(FREDCPI$DATE)
  bottom.cut <- FREDCPI %>% filter(DATE >paste(x))
  final.cut <- bottom.cut %>% filter(DATE < paste(y))
  ggplot(final.cut, aes(DATE, CPIAUCSL)) + geom_line()
}
