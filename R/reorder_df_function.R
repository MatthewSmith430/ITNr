#' @title reorder_df
#'
#' @description Reorders the rows of one dataframe according to another vector (id vector)
#' @param df dataframe to reorder
#' @param col_sort column on which the rows will be reordered
#' @param reorder_data vector with the new order
#' @export
#' @return Reordered dataframe
#' @examples
#'df <- data.frame(a = letters[1:3],b = LETTERS[4:6],c = 7:9)
#'
#'reorder_data<-c("c","a","b")
#'
#'df_new<-reorder_df(df,"a",reorder_data)
#'
#'df_new
#'
reorder_df<-function(df,col_sort,reorder_data){

  df_new<-df[order(match(df[, col_sort],reorder_data)),]
}



