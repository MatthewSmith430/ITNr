#' @title round_df
#'
#' @description This function rounds the numeric variables in a dataframe containing numeric and non-numeric data
#' @param x dataframe
#' @param digits digits to round to
#' @export
#' @return Dataframe with rounded numbers
#' @examples
#' ##Create dataframe
#' ID = c("a","b","c","d","e")
#' Value1 = c(3.445662,6.44566,8.75551,1.114522,1.5551)
#' Value2 = c(8.2,1.7,6.4,19.45459,10.34524)
#' df<-data.frame(ID,Value1,Value2)
#'
#' ##Round to 2 digits
#' rounddf<-round_df(df,2)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
