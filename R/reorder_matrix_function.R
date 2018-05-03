#' @title reorder_matrix
#'
#' @description This function reorders matrix rows and columns based on names of another vector
#' @param mat matrix
#' @param new_order characters with new order for rows & columns (names)
#' @export
#' @return reordered matrix
#' @examples
#' ##Create matrix
#' M<-replicate(5, rnorm(5))
#' rownames(M)<-LETTERS[1:5]
#' colnames(M)<-LETTERS[1:5]
#'
#' ##New order
#' reorder<-c("E","C","A","B","E")
#'
#' new_order_matrix<-reorder_matrix(M,reorder)
#'
reorder_matrix<-function(mat, new_order){
  M <- mat[order(new_order), order(new_order)]

  return(M)
}
