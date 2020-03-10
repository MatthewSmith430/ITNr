#' @title Individual/Node level E-I Index
#'
#' @description This function calculates the E-I Index (External-internal) at the individual/node level
#' @param gs igraph object
#' @param attrname Attribute name
#' @export
#' @return Group level results dataframe
#' @examples
#' require(igraph)
#' ##Create random network (igraph object)
#' gs<-erdos.renyi.game(30,0.05,directed = TRUE)
#'
#' ##Add vertex names
#' V(gs)$name<-1:vcount(gs)
#'
#' ## Add an attribute
#' V(gs)$letters<- rep(LETTERS[1:5],6)
#'
#' ##Calculate the Individual E-I Results
#' EI_IND_DATAFRAME<-ei_ind(gs,"letters")
#'

ei_ind<-function(gs,attrname){
  FULL_CENT<-data.frame(name=igraph::V(gs)$name,
                        out.degree=igraph::degree(gs,mode = "out"),
                        in.degree=igraph::degree(gs,mode = "in"),
                        total.degree=igraph::degree(gs,mode = "total"))

  GROUP<-igraph::get.vertex.attribute(gs,attrname)
  GROUP<-unique(GROUP)
  num_group<-length(GROUP)

  GROUP_MEM<-igraph::get.data.frame(gs,what="vertices")
  H<-c("name",attrname)
  GROUP_MEM<-GROUP_MEM[,H]
  colnames(GROUP_MEM)<-c("name","group")
  GROUP_CENT_LIST<-list()

  for (i in 1:num_group){
    G<-GROUP[[i]]
    GM<-dplyr::filter(GROUP_MEM,GROUP_MEM$group==G)
    SUB<-igraph::induced_subgraph(gs,GM$name)
    GC<-data.frame(name=igraph::V(SUB)$name,
                   group.out.degree=igraph::degree(SUB,mode = "out"),
                   group.in.degree=igraph::degree(SUB,mode = "in"),
                   group.total.degree=igraph::degree(SUB,mode = "total"),
                   group=rep(G,igraph::vcount(SUB)))


    GROUP_CENT_LIST[[i]]<-GC
  }

  GROUP_CENT_DF<-plyr::ldply(GROUP_CENT_LIST,data.frame)

  FULL_DF<-merge(FULL_CENT,GROUP_CENT_DF,by="name")

  FULL_DF$Internal<-FULL_DF$group.total.degree
  FULL_DF$External<-FULL_DF$total.degree-FULL_DF$group.total.degree

  FULL_DF$EI<-(FULL_DF$External-FULL_DF$Internal)/FULL_DF$total.degree

  return(FULL_DF)
}

