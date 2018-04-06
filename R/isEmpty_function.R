#' @title isEmpty
#' @description This function check whether data is numeric(0) and give returns an NA if this is true and the value of the data otherwise.
#' @param x Data
#' @export
#' @return NA or the data

isEmpty<-function(x){
  if(length(x)==0) {
    return(NA)
  } else {
    return(x)
  }
}
