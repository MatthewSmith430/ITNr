#' @title abs_diff_mat
#'
#' @description This takes a dataframe of node attributes and convert one into a absolute difference matrix
#' @param DF Dataframe of node attribute
#' @param attrname names of the attribute from the dataframe to create the matrix for.
#' @export
#' @return Absolute difference matrix
abs_diff_mat<-function(DF,attrname){
  names<-DF$id
  num_nodes<-length(DF$id)
  attr<-dplyr::select(DF,attrname)
  colnames(attr)<-"value"
  MAT1<-abs(outer(attr$value, attr$value, "-"))
  colnames(MAT1)<-names
  rownames(MAT1)<-names
  return(MAT1)
}
