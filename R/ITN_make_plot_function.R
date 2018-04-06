#' @title Single Clean ITN Plot
#'
#' @description This function plots a single/clean ITN
#' @param gs International Trade Network - igraph object
#' @param LABEL Should labels be present - TRUE/FALSE
#' @param REGION Should nodes be coloured on the basis of region TRUE/FALSE
#' @export
#' @return Panel of ITN plots
#' @examples\donttest{
#' ##Load graph
#' data("ELEnet16")
#'
#' ##Otherwise download data from WITS and create an
#' ##International Trade Network using WITSclean()
#'
#' ##Plot the network - No Label, colour by region
#' ITN_plot_example<-ITN_make_plot(ELEnet16,FALSE,TRUE)
#'}
ITN_make_plot<-function(gs,LABEL,REGION){
  gNET<-intergraph::asNetwork(gs)
  REG<-igraph::V(gs)$regionNAME
  REG<-as.vector(REG)
  region<-gsub("all income levels", "", REG)
  regionCOLOUR<-gsub("\\(|\\)", "", region)

  CENT<-ITNcentrality(gs)
  WO<-CENT$Weighted.Out.Degree
  WO<-as.vector(WO)
  WO<-as.numeric(WO)

  if (REGION==TRUE){
    if (LABEL==TRUE){
      GGally::ggnet2(gNET,
                     node.size=WO*2,node.color = regionCOLOUR,color.palette = "Set1",
                     color.legend = "Region",label = TRUE,
                     label.size = 2.5,edge.size = igraph::E(gs)$weight,
                     edge.color = c("color", "grey50"),arrow.size =5 )+
        ggplot2::guides(size = FALSE)
    }else{
      GGally::ggnet2(gNET,
                     node.size=WO*2,node.color = regionCOLOUR,color.palette = "Set1",
                     color.legend = "Region",
                     edge.size = igraph::E(gs)$weight,
                     edge.color = c("color", "grey50"),arrow.size =5 )+
        ggplot2::guides(size = FALSE)

    }

  }else{
    if (LABEL==TRUE){
      GGally::ggnet2(gNET,
                     size = WO*2,node.color = "#E41A1C",
                     label = TRUE,label.size = 2.5,edge.size = igraph::E(gs)$weight,
                     edge.color =  "grey50",arrow.size=5)+
        ggplot2::guides(color = FALSE, size = FALSE)
    } else{
      GGally::ggnet2(gNET,
                     size = WO*2,node.color = "#E41A1C",
                     label = FALSE,edge.size = igraph::E(gs)$weight,
                     edge.color =  "grey50",arrow.size=5)+
        ggplot2::guides(color = FALSE, size = FALSE)
    }

  }


}
