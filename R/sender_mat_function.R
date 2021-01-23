#' @title sender_mat
#'
#' @description This takes a dataframe of node attributes and convert one into a matrix of sender attributes
#' @param DF Dataframe of node attribute
#' @param attrname names of the attribute from the dataframe to create the matrix for.
#' @export
#' @return Sender matrix
sender_mat<-function(DF,attrname){
  names<-DF$id
  num_nodes<-length(DF$id)

  attr<-dplyr::select(DF,dplyr::all_of(attrname))
  colnames(attr)<-"value"
  x<-attr$value
  M2<-matrix(rep(x,each=num_nodes), ncol=num_nodes, byrow=TRUE)

  colnames(M2)<-names
  rownames(M2)<-names
  return(M2)
}
