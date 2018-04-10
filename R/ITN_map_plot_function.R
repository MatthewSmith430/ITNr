#' @title ITN plot on world map
#'
#' @description This function plots the international trade network on a world map
#' @param gs International Trade Network - igraph object
#' @export
#' @return Plot of the ITN on world map
#' @examples
#' require(maps)
#' ##Load the ITN
#' data(ELEnet16)
#'
#' ## Plot ITN on map - node size based on outdegree
#' ITN_map_plot(ELEnet16)
ITN_map_plot<-function(gs){
  cap_lat_lon<-base::get("cap_lat_lon")
  NAME<-igraph::V(gs)$name
  LATLONGlist<-list()
  for (i in 1:length(NAME)){
    Hcode<-NAME[[i]]
    FILTERgeo<-dplyr::filter(cap_lat_lon,cap_lat_lon$Country.Code==Hcode)
    LAT<-isEmpty(FILTERgeo$Latitude[1])
    LONG<-isEmpty(FILTERgeo$Longitude[1])
    LL<-data.frame(LAT,LONG)
    colnames(LL)<-c("LAT","LONG")
    LATLONGlist[[i]]<-LL
    }

  LATLONGdf<-plyr::ldply(LATLONGlist,data.frame)
  rownames(LATLONGdf)<-NAME
  LATLONGdf$name<-rownames(LATLONGdf)

  DISdata<-LATLONGdf
  LATLONGdf[is.na(LATLONGdf)]<-"na"
  DISdata2<-dplyr::filter(LATLONGdf,LAT!="na")

  NOll<-dplyr::filter(LATLONGdf,LATLONGdf$LAT=="na")

  NOllnode<-NOll$name
  gs2<-igraph::delete_vertices(gs, NOllnode)

  gnet<-intergraph::asNetwork(gs2)

  network::set.vertex.attribute(gnet,"lat",as.numeric(DISdata2$LAT))
  network::set.vertex.attribute(gnet,"lon",as.numeric(DISdata2$LONG))

  #gnet %v% "lon"<- as.numeric(DISdata2$LONG)
  #gnet %v% "lat"<- as.numeric(DISdata2$LAT)

  world1 <- ggplot2::fortify(maps::map("world", plot = FALSE, fill = TRUE))

  world <- ggplot2::ggplot(world1,ggplot2:: aes(x = world1$long, y = world1$lat)) +
    ggplot2::geom_polygon(ggplot2::aes(group = world1$group),
                          color = "grey65",#country border colour
                          fill = "#f9f9f9", size = 0.2)

  GGally::ggnetworkmap(world, gnet,size = 2,
                       node.color = "blue",
                       segment.color = "#AA555555")
}
