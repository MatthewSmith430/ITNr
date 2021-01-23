#' @title receiver_mat
#'
#' @description This takes a dataframe of node attributes and convert one into a matrix of receiver attributes
#' @param DF Dataframe of node attribute
#' @param attrname names of the attribute from the dataframe to create the matrix for.
#' @export
#' @return Receiver matrix
receiver_mat<-function(DF,attrname){
  names<-DF$id
  num_nodes<-length(DF$id)

  attr<-dplyr::select(DF,dplyr::all_of(attrname))
  colnames(attr)<-"value"
  x<-attr$value
  M1<-matrix(rep(x,each=num_nodes),nrow=num_nodes)

  colnames(M1)<-names
  rownames(M1)<-names
  return(M1)
}
