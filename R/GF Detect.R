GF_dataframe<-function(gs,attrname){
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

  V(NET)$id<-1:length(V(NET)$name)
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

  COOR_IND<-dplyr::group_by(dplyr::select(COOR_DF,CountryBROKER),CountryBROKER)
  COOR_IND<-unique(add_tally(COOR_IND))
  colnames(COOR_IND)<-c("name","Coordinator")

  REP_IND<-dplyr::group_by(dplyr::select(REP_DF,CountryBROKER),CountryBROKER)
  REP_IND<-unique(add_tally(REP_IND))
  colnames(REP_IND)<-c("name","Representative")

  GATE_IND<-dplyr::group_by(dplyr::select(GATE_DF,CountryBROKER),CountryBROKER)
  GATE_IND<-unique(add_tally(GATE_IND))
  colnames(GATE_IND)<-c("name","Gatekeeper")

  ITIN_IND<-dplyr::group_by(dplyr::select(ITIN_DF,CountryBROKER),CountryBROKER)
  ITIN_IND<-unique(add_tally(ITIN_IND))
  colnames(ITIN_IND)<-c("name","Consultant")

  LIA_IND<-dplyr::group_by(dplyr::select(LIA_DF,CountryBROKER),CountryBROKER)
  LIA_IND<-unique(add_tally(LIA_IND))
  colnames(LIA_IND)<-c("name","Liaison")

  MERGE1<-merge(GROUP_ID,COOR_IND,by.all="name",all.x=TRUE)
  MERGE2<-merge(MERGE1,REP_IND,by.all="name",all.x=TRUE)
  MERGE3<-merge(MERGE2,GATE_IND,by.all="name",all.x=TRUE)
  MERGE4<-merge(MERGE3,ITIN_IND,by.all="name",all.x=TRUE)
  TOTAL_DF<-merge(MERGE4,LIA_IND,by.all="name",all.x=TRUE)
  TOTAL_DF[is.na(TOTAL_DF)]<-0

  TOTAL_DF$Total<-rowSums(TOTAL_DF[,3:7])

  return(TOTAL_DF)
}
