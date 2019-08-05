#' @title Gould & Fernandez (1989) Brokerage Roles path list
#'
#' @description This function calculates returns all the Gould & Fernandez (1989) brokerage paths for an attribute
#' @param gs International Trade Network - igraph object
#' @param attrname Attribute name
#' @export
#' @return List of path dataframes: Coordinator, Representative, Gatekeeper, Consultant & Liaison
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
#' ##Calculate the GF results
#' GF_PATH<-GF_path_list(gs,"letters")
#'

GF_path_list<-function(gs,attrname){
  sub<-igraph::make_graph(c(1,2,2,3),directed=TRUE)
  NET<-igraph::simplify(gs)
  H<-igraph::subgraph_isomorphisms(sub, NET)
  EL<-list()
  for (i in 1:length(H)){
    R<-H[[i]]
    R2<-as.vector(R)
    EL[[i]]<-R2
  }
  EL2<-do.call(rbind.data.frame, EL)
  colnames(EL2)<-c("1","2","3")

  ISO<-list()
  for (i in 1:length(EL)){
    VEC<-EL[[i]]
    subCheck<-igraph::induced_subgraph(NET, VEC)
    ISO[[i]]<-igraph::ecount(subCheck)
  }
  DFedges<-suppressWarnings(purrr::map_df(ISO,data.frame))
  colnames(DFedges)<-"Number of Edges"
  DATA<-cbind(EL2,DFedges)

  dat<-DATA
  dat.sort<-t(apply(dat, 1, sort))
  DATA2<- dat[!duplicated(dat.sort),]

  #DATA2<-DATA2[DATA2$`Number of Edges` == 2,]

  igraph::V(NET)$id<-1:length(igraph::V(NET)$name)
  Vert<-igraph::get.data.frame(NET, what = "vertices")

  DATA2$`1`<-Vert$name[match(DATA2$`1`,Vert$id)]
  DATA2$`2`<-Vert$name[match(DATA2$`2`,Vert$id)]
  DATA2$`3`<-Vert$name[match(DATA2$`3`,Vert$id)]

  colnames(DATA2)<-c("Country1","CountryBROKER","Country2","NoEdges")

  NEWDF<-as.data.frame(DATA2,stringsAsFactors=FALSE)
  CONNECT_LIST<-list()
  for (j in 1:length(NEWDF$Country1)){
    H<-NEWDF[j,]
    HC<-(gs[H$Country1,H$Country2])
    HCT<-HC>0
    CONNECT_LIST[[j]]<-HCT
  }

  NEWDF$CHECK<-unlist(CONNECT_LIST)

  NEWDF3<-dplyr::filter(NEWDF,CHECK!=TRUE)

  NEWDF3$CHECK<-NULL


  CONNECT_LIST_2<-list()
  for (k in 1:length(NEWDF3$Country1)){
    H<-NEWDF3[k,]
    HC<-(gs[H$Country2,H$Country1])
    HCT<-HC>0
    CONNECT_LIST_2[[k]]<-HCT
  }

  NEWDF3$CHECK<-unlist(CONNECT_LIST_2)

  NEWDF2<-dplyr::filter(NEWDF3,CHECK!=TRUE)

  NEWDF2$CHECK<-NULL


  NEWDF2$REGION1<-NEWDF2$Country1
  NEWDF2$REGION_BROKER<-NEWDF2$CountryBROKER
  NEWDF2$REGION2<-NEWDF2$Country2
  GN1<-c("name",attrname)
  GROUP_ID<-Vert[,GN1]
  colnames(GROUP_ID)<-c("name","group")

  NEWDF2$REGION1<-GROUP_ID$group[match(NEWDF2$Country1,
                                       GROUP_ID$name)]
  NEWDF2$REGION_BROKER<-GROUP_ID$group[match(NEWDF2$CountryBROKER,
                                             GROUP_ID$name)]
  NEWDF2$REGION2<-GROUP_ID$group[match(NEWDF2$Country2,
                                       GROUP_ID$name)]


  COOR_DF<-dplyr::filter(NEWDF2,
                         NEWDF2$REGION1==NEWDF2$REGION_BROKER&
                           NEWDF2$REGION_BROKER==NEWDF2$REGION2)

  GATE_DF<-dplyr::filter(NEWDF2,NEWDF2$REGION1!=NEWDF2$REGION_BROKER&
                           NEWDF2$REGION_BROKER==NEWDF2$REGION2)

  REP_DF<-dplyr::filter(NEWDF2,NEWDF2$REGION1==NEWDF2$REGION_BROKER&
                          NEWDF2$REGION_BROKER!=NEWDF2$REGION2)

  ITIN_DF<-dplyr::filter(NEWDF2,NEWDF2$REGION1==NEWDF2$REGION2&
                           NEWDF2$REGION1!=NEWDF2$REGION_BROKER)

  LIA_DF<-dplyr::filter(NEWDF2,NEWDF2$REGION1!=NEWDF2$REGION_BROKER&
                          NEWDF2$REGION_BROKER!=NEWDF2$REGION2&
                          NEWDF2$REGION1!=NEWDF2$REGION2)



  BROKERAGE_PATH_LIST<-list(Coordinator=COOR_DF,
                            Representative=REP_DF,
                            Gatekeeper=GATE_DF,
                            Consultant=ITIN_DF,
                            Liaison=LIA_DF)

  return(BROKERAGE_PATH_LIST)
}
