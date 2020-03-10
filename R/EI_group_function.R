#' @title Group level E-I Index
#'
#' @description This function calculates the E-I Index (External-internal) at the group/attribute level
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
#' EI_GROUP_DATAFRAME<-ei_group(gs,"letters")
#'
ei_group<-function(gs,attrname){
  MM<-mixing_matrix_igraph(gs,
                           attrname)
  #(External-Internal)/External+Internal
  MM1<-MM
  diag(MM1)<-0


  GRAPH_INTERNAL<-sum(diag(MM))
  GRAPH_EXTERNAL<-sum(rowSums(MM1))
  #GRAPH_EXTERNAL<-sum(colSums(MM1))

  graph_EI<-(GRAPH_EXTERNAL - GRAPH_INTERNAL)/(GRAPH_EXTERNAL + GRAPH_INTERNAL)
  num_groups<-dim(MM)[1]
  group_name<-colnames(MM)
  INTERNAL_LIST<-diag(MM)
  RS<-rowSums(MM1)
  CS<-colSums(MM1)
  GROUP_DF_list<-list()
  for (i in 1:num_groups){
    name<-group_name[i]
    INTER<-INTERNAL_LIST[i]
    EXTER<-RS[i]+CS[i]
    EIG<-(EXTER-INTER)/(EXTER+INTER)
    GROUP_DF<-data.frame(group=name,
                         external.ties=EXTER,
                         internal.ties=INTER,
                         group.ei=EIG)
    GROUP_DF_list[[i]]<-GROUP_DF
  }
  group_EI<-plyr::ldply(GROUP_DF_list,data.frame)

  group_EI<-round_df(group_EI,4)

}
