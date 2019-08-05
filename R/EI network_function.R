#' @title Network level E-I Index
#'
#' @description This function calculates the E-I Index (External-internal) at the network level
#' @param gs igraph object
#' @param attrname Attribute name
#' @export
#' @return Group level results dataframe
#' @examples
#' require(igraph)
#' ##Create random network (igraph object)
#' gs<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Add vertex names
#' V(gs)$name<-1:vcount(gs)
#'
#' ## Add an attribute
#' V(gs)$letters<- rep(LETTERS[1:5],15)
#'
#' ##Calculate the Group E-I Results
#' EI_NETWORK<-ei_network(gs,"letters")
#'
ei_network<-function(gs,attrname){
  MM<-mixing_matrix_igraph(gs,
                           attrname)
  INT<-sum(diag(MM))
  #(External-Internal)/External+Internal
  MM1<-MM
  diag(MM1)<-0

  EXT<-sum(rowSums(MM1))

  denominator<-EXT+INT
  numerator<-EXT-INT

  EI_NET<-numerator/denominator

  EI_NET<-round(EI_NET,4)
  return(EI_NET)

}
